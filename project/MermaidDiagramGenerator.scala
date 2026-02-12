import sbt._
import sbt.Keys._
import scala.io.Source

object MermaidDiagramGenerator extends AutoPlugin {

  override def trigger = allRequirements

  object autoImport {
    val generateMermaidDiagrams = taskKey[Unit]("Generate Mermaid class diagrams from sealed traits")
    val mermaidDiagramGroups = settingKey[Map[String, Seq[String]]]("Groups of types to diagram together")
    val mermaidOutputDir = settingKey[File]("Output directory for mermaid diagrams")
    val mermaidSourceDirs = settingKey[Seq[File]]("Source directories to scan for sealed traits")
  }

  import autoImport._

  override lazy val projectSettings = Seq(
    mermaidDiagramGroups := Map(
      "CoreTypes" -> Seq(
        "com.phasmidsoftware.number.core.numerical.NumberLike",
        "com.phasmidsoftware.number.core.inner.Factor"
      ),
      "AlgebraTypes" -> Seq(
        "com.phasmidsoftware.number.algebra.core.Numeric",
        "com.phasmidsoftware.number.algebra.core.Renderable"
        // Remove TypeSafe if nothing extends it
      ),
      "ExpressionTypes" -> Seq(
        "com.phasmidsoftware.number.expression.expr.Expression"
      )
      // Remove TopLevelTypes and DimensionTypes
    ),

    mermaidOutputDir := baseDirectory.value / "docs" / "diagrams",

    // Scan all module source directories
    mermaidSourceDirs := Seq(
      baseDirectory.value / "core" / "src" / "main" / "scala",
      baseDirectory.value / "algebra" / "src" / "main" / "scala",
      baseDirectory.value / "expression" / "src" / "main" / "scala",
      baseDirectory.value / "parse" / "src" / "main" / "scala",
      baseDirectory.value / "top" / "src" / "main" / "scala",
      baseDirectory.value / "dimensions" / "src" / "main" / "scala"
    ),

    generateMermaidDiagrams := {
      val log = streams.value.log
      val diagramGroups = mermaidDiagramGroups.value
      val outputDir = mermaidOutputDir.value
      val sourceDirs = mermaidSourceDirs.value

      log.info(s"Scanning source directories: ${sourceDirs.mkString(", ")}")

      // Find all Scala files across all source directories
      val scalaFiles = sourceDirs.flatMap { dir =>
        if (dir.exists()) (dir ** "*.scala").get
        else {
          log.warn(s"Source directory does not exist: $dir")
          Seq.empty[File]
        }
      }

      log.info(s"Found ${scalaFiles.size} Scala files")

      // Parse sealed traits and their subclasses, tracking package
      case class TypeInfo(simpleName: String, packageName: String, subclasses: Seq[String])

      def parseFile(file: File): Seq[TypeInfo] = {
        val content = Source.fromFile(file).mkString
        // Remove block comments and line comments
        val noBlockComments = content.replaceAll("/\\*.*?\\*/", " ")
        val noComments = noBlockComments.replaceAll("//.*", "")

        var currentPackage: String = ""
        val hierarchies = scala.collection.mutable.Map[String, scala.collection.mutable.ListBuffer[String]]()

        // Find package declaration
        val packagePattern = """package\s+([\w.]+)""".r
        packagePattern.findFirstMatchIn(content).foreach { m =>
          currentPackage = m.group(1)
        }

        // Find all trait/class definitions (that don't extend anything)
        val traitPattern = """(?:sealed\s+)?(?:trait|class|abstract\s+class)\s+([A-Z]\w+)(?:\[.*?\])?\s*(?:\(.*?\))?\s*(?:\{|$)""".r
        traitPattern.findAllMatchIn(noComments).foreach { m =>
          val name = m.group(1)
          // Only add if the context doesn't have "extends" before the next line break or {
          val contextStart = Math.max(0, m.start - 100)
          val contextEnd = Math.min(noComments.length, m.end + 100)
          val context = noComments.substring(contextStart, contextEnd)

          if (!context.substring(0, m.end - contextStart).contains("extends")) {
            hierarchies.getOrElseUpdate(name, scala.collection.mutable.ListBuffer())
          }
        }

        // Find all class/object/trait extensions

        // Better pattern (non-greedy match before extends):
        val extendsPattern = """(?:case\s+)?(?:class|object|trait)\s+([A-Z]\w+)(?:\[.*?\])?.*?extends\s+([A-Z]\w+)""".r
//        val extendsPattern = """(?:case\s+)?(?:class|object|trait)\s+([A-Z]\w+)(?:\[.*?\])?\s*(?:\(.*?\))?\s+extends\s+([A-Z]\w+)""".r
        extendsPattern.findAllMatchIn(noComments).foreach { m =>
          val childName = m.group(1)
          val parentName = m.group(2)
          if (file.getName == "Rational.scala" || parentName == "NumberLike") {
            log.debug(s"DEBUG ${file.getName}: Found $childName extends $parentName")
          }
          hierarchies.getOrElseUpdate(parentName, scala.collection.mutable.ListBuffer()) += childName
        }

        // Also look for "with" mixins
        val withPattern = """with\s+([A-Z]\w+)""".r

        // For each child found, check if it uses "with" for additional traits
        extendsPattern.findAllMatchIn(noComments).foreach { m =>
          val childName = m.group(1)
          val matchEnd = m.end
          // Look for "with" clauses after this extends
          val afterExtends = noComments.substring(matchEnd, Math.min(noComments.length, matchEnd + 200))
          withPattern.findAllMatchIn(afterExtends.takeWhile(c => c != '{' && c != '\n')).foreach { wm =>
            val mixinName = wm.group(1)
            hierarchies.getOrElseUpdate(mixinName, scala.collection.mutable.ListBuffer()) += childName
          }
        }

        hierarchies.map { case (name, subclasses) =>
          TypeInfo(name, currentPackage, subclasses.toSeq.distinct)
        }.toSeq
      }

      // Build complete hierarchy map with fully qualified names
      val allTypeInfos = scalaFiles.flatMap(parseFile)
      val hierarchyMap = allTypeInfos.map { info =>
        val fqn = if (info.packageName.nonEmpty) s"${info.packageName}.${info.simpleName}" else info.simpleName
        fqn -> info
      }.toMap

      // After building hierarchyMap, consolidate duplicate simple names
      val consolidatedMap = hierarchyMap.groupBy { case (fqn, info) => info.simpleName }.map {
        case (simpleName, entries) =>
          if (entries.size > 1) {
            // Multiple entries with same simple name - merge their subclasses
            val allSubclasses = entries.flatMap(_._2.subclasses).toSeq.distinct
            // Pick the "real" definition (the one with empty subclasses in original map)
            val mainEntry = entries.find(_._2.subclasses.isEmpty).getOrElse(entries.head)
            val mergedInfo = mainEntry._2.copy(subclasses = allSubclasses)
            mainEntry._1 -> mergedInfo
          } else {
            entries.head
          }
      }.toMap

      // Also build a simple name index for resolving imports
      val simpleNameMap = allTypeInfos.groupBy(_.simpleName).map { case (simpleName, infos) =>
        simpleName -> infos
      }

      log.info(s"Found ${consolidatedMap.size} traits/classes")
      log.info(s"Simple name index has ${simpleNameMap.size} entries")

      // Helper to get simple name from FQN or simple name
      def getSimpleName(name: String): String = name.split('.').last

      // Helper to resolve a parent type reference to its TypeInfo
      def resolveTypeInfo(typeName: String): Option[TypeInfo] = {
        // Try FQN first
        consolidatedMap.get(typeName).orElse {
          // Try simple name lookup
          val simpleName = getSimpleName(typeName)
          simpleNameMap.get(simpleName).flatMap(_.headOption)
        }
      }

      // Generate diagrams
      IO.createDirectory(outputDir)

      diagramGroups.foreach { case (groupName, typeNames) =>
        log.info(s"Generating diagram group: $groupName")

        val hierarchiesForGroup = typeNames.flatMap { typeName =>
          resolveTypeInfo(typeName).map { info =>
            val simpleName = getSimpleName(typeName)

            if (info.subclasses.nonEmpty) {
              log.info(s"  Found ${info.subclasses.size} subclasses for $simpleName: ${info.subclasses.mkString(", ")}")

              val relationships = info.subclasses.map { subName =>
                s"  $simpleName <|-- $subName\n  class $subName"
              }.mkString("\n")

              s"  class $simpleName\n$relationships"
            } else {
              log.warn(s"  No subclasses found for $typeName")
              s"  class $simpleName"
            }
          }.orElse {
            log.warn(s"  Type not found in hierarchy map: $typeName")
            None
          }
        }

        if (hierarchiesForGroup.nonEmpty) {
          val diagram = s"""classDiagram
                           |${hierarchiesForGroup.mkString("\n")}
                           |""".stripMargin

          val outputFile = outputDir / s"$groupName.mmd"
          IO.write(outputFile, diagram)
          log.info(s"Generated diagram: $outputFile")
        } else {
          log.warn(s"No hierarchies found for group: $groupName")
        }
      }

      log.info(s"Diagrams written to $outputDir")
    }
  )
}