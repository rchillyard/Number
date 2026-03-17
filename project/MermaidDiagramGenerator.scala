import sbt._
import sbt.Keys._

/** Generates a Mermaid class diagram from Scala source files by text-parsing
  * sealed trait/class hierarchies. Works with Scala 3 without needing runtime
  * reflection or TASTy inspection.
  *
  * Usage (from root):
  *   sbt generateMermaidDiagram
  *
  * Output:
  *   docs/diagrams/number.mmd  (embed in any markdown with ```mermaid fences)
  */
object MermaidDiagramGenerator extends AutoPlugin {

  override def trigger = noTrigger // only enabled where explicitly requested

  object autoImport {
    val generateMermaidDiagram =
      taskKey[Unit]("Generate a combined Mermaid class diagram from all Scala sources")

    val mermaidOutputFile =
      settingKey[File]("Output file for the combined Mermaid diagram")

    val mermaidSourceRoots =
      settingKey[Seq[File]]("Source root directories to scan (default: all modules under base)")

    /** Simple names of sealed types to use as diagram roots.
      * If empty, ALL sealed traits and sealed abstract classes are included.
      */
    val mermaidRootTypes =
      settingKey[Seq[String]]("Simple names of sealed types to treat as hierarchy roots (empty = all)")

    /** Simple names to exclude from the diagram entirely. */
    val mermaidExcludeTypes =
      settingKey[Set[String]]("Simple type names to exclude from the diagram")

    /** Whether to include companion/case objects in the diagram. Default: false.
      * Objects rarely add structural value and cause duplicate-name noise when
      * a companion object shares a name with its trait/class.
      */
    val mermaidIncludeObjects =
      settingKey[Boolean]("Include Scala objects (companions/case objects) in the diagram (default: false)")
  }

  import autoImport._

  override lazy val projectSettings: Seq[Setting[_]] = Seq(
    mermaidOutputFile     := baseDirectory.value / "docs" / "diagrams" / "number.mmd",
    mermaidSourceRoots    := Seq(baseDirectory.value),
    mermaidRootTypes      := Seq.empty,
    mermaidExcludeTypes   := Set.empty,
    mermaidIncludeObjects := false,

    generateMermaidDiagram := {
      val log            = streams.value.log
      val outFile        = mermaidOutputFile.value
      val srcRoots       = mermaidSourceRoots.value
      val rootTypes      = mermaidRootTypes.value.toSet
      val excluded       = mermaidExcludeTypes.value
      val includeObjects = mermaidIncludeObjects.value

      // -----------------------------------------------------------------------
      // 1. Collect all .scala files under the given source roots
      // -----------------------------------------------------------------------
      val scalaFiles: Seq[File] =
        srcRoots.flatMap(root => (root ** "*.scala").get)
          .filter(f => !f.getPath.contains("/project/") && !f.getPath.contains("\\project\\"))
          .distinct

      log.info(s"[MermaidDiagramGenerator] Scanning ${scalaFiles.size} Scala source files...")

      // -----------------------------------------------------------------------
      // 2. Parse each file to extract type definitions and their parents
      // -----------------------------------------------------------------------
      case class TypeInfo(
                           simpleName: String,
                           kind: String,        // "trait" | "abstract class" | "class" | "case class" | "object"
                           isSealed: Boolean,
                           parents: Seq[String] // simple names only
                         )

      // Numeric rank for deduplication: lower = preferred when same name appears
      // multiple times (e.g. trait Foo and object Foo in the same codebase)
      def kindRank(kind: String): Int = kind match {
        case "trait"          => 0
        case "abstract class" => 1
        case "class"          => 2
        case "case class"     => 3
        case "object"         => 4
        case _                => 5
      }

      val TypeDef = (
        """(?m)^\s*""" +
          """((?:(?:sealed|abstract|final|case|implicit|private|protected|override)\s+)*)""" +
          """(trait|class|object)\s+""" +
          """([A-Z][A-Za-z0-9_$]*)""" +
          """[^\{]*?""" +
          """(?:extends\s+([^\{]+?))?""" +
          """(?:\s*\{|\s*$)"""
        ).r

      def stripComments(src: String): String = {
        val noBlock = """/\*[\s\S]*?\*/""".r.replaceAllIn(src, " ")
        """//[^\n]*""".r.replaceAllIn(noBlock, "")
      }

      def parseParents(clause: String): Seq[String] = {
        if (clause == null || clause.isBlank) Seq.empty
        else
          clause
            .split("""with\b""")
            .map(_.trim)
            .filter(_.nonEmpty)
            .map { part =>
              val name = part.replaceAll("""[\[\(].*""", "").trim
              name.split("""\.""").last.trim
            }
            .filter(n => n.nonEmpty && n.head.isUpper)
      }

      def parseFile(file: File): Seq[TypeInfo] = {
        val raw = IO.read(file)
        val src = stripComments(raw)
        TypeDef.findAllMatchIn(src).map { m =>
          val mods      = m.group(1).toLowerCase
          val kw        = m.group(2)
          val name      = m.group(3)
          val extClause = Option(m.group(4)).getOrElse("")
          val isSealed   = mods.contains("sealed")
          val isAbstract = mods.contains("abstract")
          val isCase     = mods.contains("case")
          val kind = (kw, isAbstract, isCase) match {
            case ("trait", _, _)    => "trait"
            case ("class", true, _) => "abstract class"
            case ("class", _, true) => "case class"
            case ("class", _, _)    => "class"
            case ("object", _, _)   => "object"
            case _                  => kw
          }
          TypeInfo(name, kind, isSealed, parseParents(extClause))
        }.toSeq
      }

      val rawTypes: Seq[TypeInfo] =
        scalaFiles.flatMap(parseFile)
          .filterNot(t => excluded.contains(t.simpleName))
          .filterNot(t => !includeObjects && t.kind == "object")

      // -----------------------------------------------------------------------
      // 3. Deduplicate: when the same simple name appears with multiple kinds
      //    (e.g. a trait and its companion object, or the same trait defined
      //    in multiple modules), keep the highest-priority kind and merge parents.
      // -----------------------------------------------------------------------
      val deduped: Seq[TypeInfo] = {
        val grouped = rawTypes.groupBy(_.simpleName)
        grouped.values.map { copies =>
          val best      = copies.minBy(c => kindRank(c.kind))
          val isSealed2 = copies.exists(_.isSealed)
          val parents   = copies.flatMap(_.parents).distinct
          best.copy(isSealed = isSealed2, parents = parents)
        }.toSeq.sortBy(_.simpleName)
      }

      log.info(s"[MermaidDiagramGenerator] Found ${deduped.size} distinct type definitions.")

      // -----------------------------------------------------------------------
      // 4. Build the inheritance graph restricted to known types
      // -----------------------------------------------------------------------
      val knownNames: Set[String] = deduped.map(_.simpleName).toSet

      // child -> distinct parents within the project
      val edges: Map[String, Seq[String]] =
        deduped.map { t =>
          t.simpleName -> t.parents.filter(knownNames.contains).distinct
        }.toMap

      // All descendants reachable from a root (depth-first)
      def reachableFrom(root: String): Set[String] = {
        val childrenOf: Map[String, Seq[String]] =
          edges.toSeq
            .flatMap { case (child, parents) => parents.map(p => p -> child) }
            .groupBy(_._1)
            .map { case (k, v) => k -> v.map(_._2) }

        def dfs(name: String, visited: Set[String]): Set[String] =
          if (visited.contains(name)) visited
          else childrenOf.getOrElse(name, Seq.empty)
            .foldLeft(visited + name)((acc, c) => dfs(c, acc))

        dfs(root, Set.empty)
      }

      // -----------------------------------------------------------------------
      // 5. Decide which types to include
      // -----------------------------------------------------------------------
      val sealedNames: Set[String] = deduped.filter(_.isSealed).map(_.simpleName).toSet

      val includedNames: Set[String] =
        if (rootTypes.nonEmpty) {
          rootTypes.flatMap(reachableFrom)
        } else {
          val fromSealed = sealedNames.flatMap(reachableFrom)
          def ancestors(name: String): Set[String] = {
            val ps = edges.getOrElse(name, Seq.empty).filter(knownNames.contains)
            ps.toSet ++ ps.flatMap(ancestors)
          }
          fromSealed ++ fromSealed.flatMap(ancestors)
        }

      val includedTypes: Seq[TypeInfo] =
        deduped.filter(t => includedNames.contains(t.simpleName))

      log.info(s"[MermaidDiagramGenerator] Including ${includedTypes.size} types in diagram.")

      // -----------------------------------------------------------------------
      // 6. Render Mermaid classDiagram syntax
      // -----------------------------------------------------------------------
      def mermaidStereotype(kind: String): String = kind match {
        case "trait"          => "<<trait>>"
        case "abstract class" => "<<abstract>>"
        case "object"         => "<<object>>"
        case _                => ""
      }

      val includedNameSet = includedTypes.map(_.simpleName).toSet

      // Collect (parent, child) edge pairs, deduplicated
      val edgePairs: Seq[(String, String)] =
        includedTypes.flatMap { child =>
          edges.getOrElse(child.simpleName, Seq.empty)
            .filter(includedNameSet.contains)
            .map(parent => (parent, child.simpleName))
        }.distinct

      val sb = new StringBuilder
      sb.append("classDiagram\n")
      sb.append("  %% Auto-generated by MermaidDiagramGenerator -- do not edit by hand\n\n")

      // Class declarations with stereotypes
      includedTypes.foreach { t =>
        val stereo = mermaidStereotype(t.kind)
        sb.append(s"  class ${t.simpleName}\n")
        if (stereo.nonEmpty)
          sb.append(s"  ${t.simpleName} : $stereo\n")
      }

      sb.append("\n")

      // Inheritance edges
      edgePairs.foreach { case (parent, child) =>
        sb.append(s"  $parent <|-- $child\n")
      }

      // -----------------------------------------------------------------------
      // 7. Write output
      // -----------------------------------------------------------------------
      IO.createDirectory(outFile.getParentFile)
      IO.write(outFile, sb.toString())
      log.success(
        s"[MermaidDiagramGenerator] Diagram written to: $outFile  " +
          s"(${includedTypes.size} types, ${edgePairs.size} edges)"
      )
    }
  )
}