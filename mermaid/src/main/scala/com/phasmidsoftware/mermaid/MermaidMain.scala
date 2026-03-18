package com.phasmidsoftware.mermaid

import tastyquery.Contexts.*
import tastyquery.Names.*
import tastyquery.Symbols.*
import tastyquery.Types.*
import tastyquery.Modifiers
import tastyquery.jdk.ClasspathLoaders

import java.nio.file.{Files, Path, Paths}
import scala.util.Try

/** Entry point invoked by MermaidDiagramGenerator as a subprocess.
  *
  * args(0)       = output directory
  * args(1)       = max members per type box
  * args(2)       = "true"/"false" -- include objects
  * args(3)       = "true"/"false" -- explicit groups mode
  * args(4..N-1)  = "moduleName:/path/to/classes"  OR  "groupName:fqn1,fqn2,..."
  * args(N)       = "--cp"
  * args(N+1..)   = classpath entries (stdlib + dep jars)
  */
object MermaidMain {

  val version = "0.1.0"

  def main(rawArgs: Array[String]): Unit = {
    val args = rawArgs.toSeq

    if (args.length < 4) {
      System.err.println("Usage: MermaidMain <outDir> <maxMembers> <inclObjects> <explicitGroups> [moduleOrGroup...] --cp [cpEntry...]")
      sys.exit(1)
    }
    else
      System.out.println(s"MermaidMain $version")

    val outDir       = Paths.get(args(0))
    val maxMembers   = args(1).toInt
    val inclObjects  = args(2) == "true"
    val explicitMode = args(3) == "true"

    val cpIdx      = args.indexWhere(_ == "--cp")
    val moduleArgs = (if (cpIdx > 4) args.slice(4, cpIdx) else args.drop(4).takeWhile(_ != "--cp")).toSeq
    val cpEntries  = (if (cpIdx >= 0) args.drop(cpIdx + 1) else Seq.empty).toSeq

    val moduleEntries: Seq[(String, String)] = moduleArgs.map { arg =>
      val colon = arg.indexOf(':')
      (arg.take(colon), arg.drop(colon + 1))
    }

    val allClassesDirs: Seq[Path] =
      moduleEntries.map(_._2).flatMap { v =>
        v.split(',').map(s => Paths.get(s)).filter(p => Files.isDirectory(p))
      }.distinct

    val fullCp: List[Path] =
      (allClassesDirs ++ cpEntries.map(s => Paths.get(s)).filter(p => Files.exists(p))).distinct.toList

    // Add JDK java.base module so tasty-query can resolve java.lang.Object etc.
    val javaBase = java.nio.file.FileSystems
      .getFileSystem(java.net.URI.create("jrt:/"))
      .getPath("modules", "java.base")
    val fullCpWithJdk = (javaBase +: fullCp).distinct

    println(s"[MermaidMain] Classpath entries: ${fullCpWithJdk.size}, output: $outDir")

    // -----------------------------------------------------------------------
    // Context
    // -----------------------------------------------------------------------
    val classpath = ClasspathLoaders.read(fullCpWithJdk)
    given ctx: Context = Context.initialize(classpath)

    // -----------------------------------------------------------------------
    // Data model
    // -----------------------------------------------------------------------
    case class MemberInfo(signature: String, isAbstract: Boolean)

    case class TypeInfo(
                         fqn:        String,
                         simpleName: String,
                         kind:       String,
                         isSealed:   Boolean,
                         parentFQNs: Seq[String],
                         members:    Seq[MemberInfo],
                         moduleName: String,
                         symbol:     Option[ClassSymbol] = None
                       )

    // -----------------------------------------------------------------------
    // Helpers
    // -----------------------------------------------------------------------

    def kindOf(cls: ClassSymbol): String = {
      if      (cls.isModuleClass)   "object"
      else if (cls.isTrait)         "trait"
      else if (cls.isAbstractClass) "abstract class"
      else if (cls.isCaseClass)     "case class"
      else                          "class"
    }

    def fqnOf(cls: ClassSymbol): String = cls.displayFullName

    val noiseNames: Set[String] = Set(
      "equals", "hashCode", "toString", "copy", "canEqual",
      "productArity", "productElement", "productIterator",
      "productPrefix", "readResolve", "writeReplace",
      "apply", "unapply", "tupled", "curried", "fromProduct",
      "<init>" // constructor noise
    )

    def typeStr(tpe: Type): String = tpe match {
      case ref: TypeRef     => ref.name.toString
      case app: AppliedType => s"${typeStr(app.tycon)}[${app.args.map { case t: Type => typeStr(t); case _ => "_" }.mkString(",")}]"
      case or: OrType       => s"${typeStr(or.first)}|${typeStr(or.second)}"
      case and: AndType     => s"${typeStr(and.first)}&${typeStr(and.second)}"
      case _: ByNameType    => "=>"
      case _                => "_"
    }

    def memberSig(sym: TermSymbol): Option[MemberInfo] = {
      if (sym.isSynthetic) return None
      val name = sym.name.toString
      if (name.startsWith("_") || name.contains("$default$") || noiseNames.contains(name)) return None
      val isAbs = sym.isAbstractMember
      val isVal = { val k = sym.kind; k == tastyquery.Modifiers.TermSymbolKind.Val || k == tastyquery.Modifiers.TermSymbolKind.LazyVal }
      val kw    = if (isVal) "val" else "def"
      Try {
        sym.declaredType match {
          case mt: MethodType =>
            val params = mt.paramNames.zip(mt.paramTypes)
              .map { case (n, t) => s"$n:${typeStr(t)}" }.mkString(",")
            val ret = mt.resultType match {
              case t: Type => typeStr(t)
              case _       => "_"
            }
            s"$kw $name($params) $ret"
          case pt: PolyType =>
            val ret = pt.resultType match {
              case t: Type => typeStr(t)
              case _       => "_"
            }
            s"$kw $name[..] $ret"
          case t: Type =>
            s"$kw $name ${typeStr(t)}"
          //          case _ =>
          //            s"$kw $name"
        }
      }.toOption.map(sig => MemberInfo(sig, isAbs))
    }

    // Convert a dotted FQN to List[Name] for findSymbolFromRoot:
    // package segments are TermNames, the final class segment is a TypeName.
    def nameParts(dotted: String): List[Name] = {
      val parts = dotted.split('.').toList
      parts.zipWithIndex.map { case (part, idx) =>
        if (idx == parts.length - 1) typeName(part)
        else termName(part)
      }
    }

    // -----------------------------------------------------------------------
    // Walk a classes directory
    // -----------------------------------------------------------------------
    val moduleClassDirs: Seq[(String, Path)] =
      if (explicitMode) {
        allClassesDirs.map(p => "explicit" -> p)
      } else {
        moduleEntries.map { case (name, path) => name -> Paths.get(path) }
          .filter { case (_, p) => Files.isDirectory(p) }
      }

    def walkDir(modName: String, classesDir: Path): Seq[TypeInfo] = {
      val tastyFiles = Files.walk(classesDir)
        .filter(p => p.toString.endsWith(".tasty"))
        .toArray.toSeq.asInstanceOf[Seq[Path]]

      tastyFiles.flatMap { tf =>
        val rel = classesDir.relativize(tf).toString
          .replace("\\", "/").stripSuffix(".tasty").replace("/", ".")
        Try {
          ctx.findSymbolFromRoot(nameParts(rel)) match {
            case cls: ClassSymbol if !cls.isSynthetic =>
              val kind       = kindOf(cls)
              val simpleName = cls.name.toString.stripSuffix("$")
              if (!inclObjects && kind == "object") {
                None
              } else {
                val members = Try(cls.declarations(using ctx)).getOrElse(Nil)
                  .collect { case ts: TermSymbol => ts }
                  .flatMap(memberSig)
                  .sortBy(m => (!m.isAbstract, m.signature))
                Some(TypeInfo(
                  fqn        = fqnOf(cls),
                  simpleName = simpleName,
                  kind       = kind,
                  isSealed   = cls.sealedChildren(using ctx).nonEmpty,
                  parentFQNs = Seq.empty,
                  members    = members,
                  moduleName = modName,
                  symbol     = Some(cls)
                ))
              }
            case _ => None
          }
        }.toOption.flatten
      }
    }

    val rawTypes  = moduleClassDirs.flatMap { case (n, p) => walkDir(n, p) }
    val byFQN     = rawTypes.groupBy(_.fqn).map { case (fqn, dups) => fqn -> dups.head }
    val knownFQNs = byFQN.keySet

    // Resolve parent FQNs using the saved ClassSymbol -- avoids re-lookup
    val typeInfos: Seq[TypeInfo] = byFQN.values.map { ti =>
      val parents = ti.symbol match {
        case Some(cls) =>
          Try(cls.parentClasses(using ctx).map(p => p.displayFullName).filter(knownFQNs.contains))
            .getOrElse(Seq.empty)
        case None => Seq.empty
      }
      ti.copy(parentFQNs = parents)
    }.toSeq

    println(s"[MermaidMain] Extracted ${typeInfos.size} distinct types.")

    // -----------------------------------------------------------------------
    // Reachability
    // -----------------------------------------------------------------------
    val edgeMap: Map[String, Seq[String]] = typeInfos.map(t => t.fqn -> t.parentFQNs).toMap
    val fqnToInfo: Map[String, TypeInfo]  = typeInfos.map(t => t.fqn -> t).toMap

    def reachableFrom(rootFQN: String): Set[String] = {
      val childrenOf: Map[String, Seq[String]] =
        edgeMap.toSeq
          .flatMap { case (child, parents) => parents.map(_ -> child) }
          .groupBy(_._1).map { case (k, v) => k -> v.map(_._2) }
      def dfs(fqn: String, visited: Set[String]): Set[String] = {
        if (visited.contains(fqn)) visited
        else childrenOf.getOrElse(fqn, Seq.empty).foldLeft(visited + fqn)((acc, c) => dfs(c, acc))
      }
      dfs(rootFQN, Set.empty)
    }

    // -----------------------------------------------------------------------
    // Groups
    // -----------------------------------------------------------------------
    val diagramGroups: Map[String, Set[String]] = {
      if (explicitMode) {
        moduleEntries.map { case (groupName, fqnsStr) =>
          val roots    = fqnsStr.split(',').toSeq
          val resolved = typeInfos.filter(t => roots.contains(t.fqn) || roots.contains(t.simpleName)).map(_.fqn)
          groupName -> resolved.flatMap(reachableFrom).toSet
        }.toMap
      } else {
        moduleClassDirs.map { case (modName, _) =>
          // Include all types in this module; edges to parents in other modules
          // are shown if the parent is in our known set (cross-module edges)
          val modFQNs = typeInfos.filter(_.moduleName == modName).map(_.fqn).toSet
          modName -> modFQNs
        }.toMap
      }
    }

    // -----------------------------------------------------------------------
    // Render .mmd files
    // -----------------------------------------------------------------------
    Files.createDirectories(outDir)

    def stereotype(kind: String): String = kind match {
      case "trait"          => "<<trait>>"
      case "abstract class" => "<<abstract>>"
      case "object"         => "<<object>>"
      case _                => ""
    }

    def sanitise(sig: String): String =
      sig.replace("[", "~").replace("]", "~").replace("<", "").replace(">", "").take(72)

    val typeInDiagrams = scala.collection.mutable.Map[String, List[String]]().withDefaultValue(Nil)

    diagramGroups.foreach { case (diagName, fqns) =>
      val included     = fqns.flatMap(fqnToInfo.get).toSeq.sortBy(_.simpleName)
      val includedFQNs = included.map(_.fqn).toSet
      val edgePairs    = included.flatMap { t =>
        // Include edges to parents in other modules too, as long as we know about them
        t.parentFQNs.filter(fqnToInfo.contains).map(p => (p, t.fqn))
      }.distinct

      val sb = new StringBuilder
      sb.append("classDiagram\n")
      sb.append(s"  %% $diagName -- auto-generated by MermaidMain, do not edit\n\n")

      included.foreach { t =>
        val st      = stereotype(t.kind)
        val members = t.members
        sb.append(s"  class ${t.simpleName}")
        if (st.nonEmpty || members.nonEmpty) {
          sb.append(" {\n")
          if (st.nonEmpty) sb.append(s"    $st\n")
          members.take(maxMembers).foreach { m =>
            val vis = if (m.isAbstract) "+" else "-"
            sb.append(s"    $vis${sanitise(m.signature)}\n")
          }
          if (members.size > maxMembers)
            sb.append(s"    ...(${members.size - maxMembers} more)\n")
          sb.append("  }\n")
        } else {
          sb.append("\n")
        }
        typeInDiagrams(t.simpleName) = diagName :: typeInDiagrams(t.simpleName)
      }

      // Collect parent nodes referenced by edges but not in this module's diagram
      val referencedParentFQNs = edgePairs.map(_._1).toSet -- includedFQNs
      val extraNodes = referencedParentFQNs.flatMap(fqnToInfo.get).toSeq.sortBy(_.simpleName)

      // Emit extra parent nodes (without members, just the stereotype)
      if (extraNodes.nonEmpty) {
        sb.append("  %% cross-module parents\n")
        extraNodes.foreach { t =>
          val st = stereotype(t.kind)
          sb.append(s"  class ${t.simpleName}")
          if (st.nonEmpty) {
            sb.append(s" {\n    $st\n  }\n")
          } else {
            sb.append("\n")
          }
        }
      }


      sb.append("\n")
      edgePairs.foreach { case (pFQN, cFQN) =>
        val p = fqnToInfo.get(pFQN).map(_.simpleName).getOrElse(pFQN.split('.').last)
        val c = fqnToInfo.get(cFQN).map(_.simpleName).getOrElse(cFQN.split('.').last)
        sb.append(s"  $p <|-- $c\n")
      }

      val outFile = outDir.resolve(s"$diagName.mmd")
      Files.writeString(outFile, sb.toString())
      println(s"[MermaidMain] $diagName.mmd  (${included.size} types, ${edgePairs.size} edges)")
    }

    // -----------------------------------------------------------------------
    // Write index.md
    // -----------------------------------------------------------------------
    val idx = new StringBuilder
    idx.append("# Type index\n\n_Auto-generated by MermaidMain -- do not edit._\n\n")
    idx.append("| Type | Kind | Module | Diagrams |\n")
    idx.append("|------|------|--------|----------|\n")
    typeInfos.sortBy(_.simpleName).foreach { t =>
      val links = typeInDiagrams(t.simpleName).distinct.sorted.map(d => s"[$d]($d.mmd)").mkString(", ")
      idx.append(s"| `${t.simpleName}` | ${t.kind} | ${t.moduleName} | $links |\n")
    }
    Files.writeString(outDir.resolve("index.md"), idx.toString())
    println(s"[MermaidMain] index.md written (${typeInfos.size} types)")
  }
}