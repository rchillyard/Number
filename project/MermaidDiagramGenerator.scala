import sbt._
import sbt.Keys._
import scala.sys.process.ProcessLogger

/** Launches the mermaid submodule's MermaidMain as a subprocess to generate
  * Mermaid class diagrams from compiled TASTy files.
  *
  * The subprocess runs on Scala 3 and uses tasty-query, which cannot run
  * inside sbt's own Scala 2.12 JVM. sbt's role here is purely to:
  *   1. Assemble the arguments (output dir, module classpath entries, full cp)
  *   2. Fork the process using the mermaid submodule's classpath
  *   3. Report success or failure
  *
  * Settings (configure in build.sbt on the root project):
  *   mermaidOutputDir      -- where .mmd files are written (default: docs/diagrams/)
  *   mermaidGroups         -- explicit groups: name -> root FQNs (empty = per-module)
  *   mermaidIncludeObjects -- include Scala objects (default: false)
  *   mermaidMembersMax     -- max members shown per type box IF any concrete members are shown "...(N more)" (default: 10)
  */
object MermaidDiagramGenerator extends AutoPlugin {

  override def trigger = noTrigger

  object autoImport {
    val generateMermaidDiagram =
      taskKey[Unit]("Generate Mermaid class diagrams from compiled TASTy (via subprocess)")

    val mermaidOutputDir =
      settingKey[File]("Output directory for .mmd files (default: docs/diagrams/)")

    /** Explicit diagram groups. Key = output filename stem, value = root FQNs or
      * simple names. If empty, one diagram per sbt module is produced automatically.
      */
    val mermaidGroups =
      settingKey[Map[String, Seq[String]]]("Explicit diagram groups: name -> root FQNs (empty = per-module)")

    val mermaidIncludeObjects =
      settingKey[Boolean]("Include Scala objects in diagrams (default: false)")

    val mermaidMembersMax =
      settingKey[Int]("Max members shown per type box (only applies IF any concrete members are shown) '...' (default: 10)")
  }

  import autoImport.*

  // Extracted into a real method so we can use early-return style via Either
  private def run(
                   log:      sbt.util.Logger,
                   outDir:   File,
                   groups:   Map[String, Seq[String]],
                   inclObj:  Boolean,
                   maxMem:   Int,
                   struct:   sbt.internal.BuildStructure,
                   refs:     Seq[sbt.ProjectRef],
                   sv:       String,
                   fullCp:   Seq[File],
                   st:       sbt.State
                 ): Unit = {

    // -------------------------------------------------------------------------
    // 1. Locate compiled classes dirs for every aggregated module
    // -------------------------------------------------------------------------
    val moduleClassDirs: Seq[(String, File)] = refs.flatMap { ref =>
      Project.getProjectForReference(ref, struct).flatMap { proj =>
        if (proj.id == "mermaid") None
        else {
          val dir = proj.base / "target" / s"scala-$sv" / "classes"
          if (dir.exists()) Some(proj.id -> dir)
          else {
            log.warn(s"[MermaidDiagramGenerator] ${proj.id}: no classes at $dir -- run compile first")
            None
          }
        }
      }
    }

    if (moduleClassDirs.isEmpty) {
      log.error("[MermaidDiagramGenerator] No compiled modules found. Run 'compile' first.")
      return
    }

    log.info(s"[MermaidDiagramGenerator] Modules: ${moduleClassDirs.map(_._1).mkString(", ")}")

    // -------------------------------------------------------------------------
    // 2. Build the argument list for MermaidMain
    // -------------------------------------------------------------------------
    val explicitMode = groups.nonEmpty

    val moduleArgs: Seq[String] =
      if (explicitMode)
        groups.toSeq.map { case (name, roots) => s"$name:${roots.mkString(",")}" }
      else
        moduleClassDirs.map { case (name, dir) => s"$name:${dir.getAbsolutePath}" }

    // Include module classes dirs in the cp so tasty-query resolves cross-module refs
    val allCpFiles: Seq[File] = (fullCp ++ moduleClassDirs.map(_._2)).distinct

    val mainArgs: Seq[String] =
      Seq(outDir.getAbsolutePath, maxMem.toString, inclObj.toString, explicitMode.toString) ++
        moduleArgs ++
        Seq("--cp") ++
        allCpFiles.map(_.getAbsolutePath)

    // -------------------------------------------------------------------------
    // 3. Locate the mermaid submodule's runtime classpath
    // -------------------------------------------------------------------------
    val mermaidCpOpt: Option[Seq[File]] =
      Project.runTask(LocalProject("mermaid") / Compile / fullClasspath, st) match {
        case Some((_, Value(cp))) => Some(cp.files)
        case Some((_, Inc(inc))) =>
          log.error(s"[MermaidDiagramGenerator] Failed to get mermaid classpath: $inc")
          None
        case None =>
          log.error("[MermaidDiagramGenerator] mermaid project not found -- is it defined in build.sbt?")
          None
      }

    mermaidCpOpt match {
      case None => // error already logged above
      case Some(mermaidCp) =>

        // -------------------------------------------------------------------------
        // 4. Fork the subprocess
        // -------------------------------------------------------------------------
        val mainClass = "com.phasmidsoftware.mermaid.MermaidMain"

        // Explicitly locate mermaid's classes dir — fullClasspath may not include it
        // when resolved via runTask in this context
        val mermaidBase = struct.allProjectRefs
          .flatMap(ref => Project.getProjectForReference(ref, struct))
          .find(_.id == "mermaid")
          .map(_.base)
        val mermaidClassesDir: File = mermaidBase
          .map(_ / "target" / s"scala-$sv" / "classes")
          .getOrElse(sys.error("mermaid subproject base not found in build structure"))

        log.info(s"[MermaidDiagramGenerator] mermaid classes: $mermaidClassesDir (exists=${mermaidClassesDir.exists()})")

        val cpString = (mermaidCp :+ mermaidClassesDir).distinct.mkString(java.io.File.pathSeparator)

        log.info(s"[MermaidDiagramGenerator] Launching $mainClass ...")

        val javaHome = sys.props.get("java.home").map(h => new File(h)).getOrElse(new File("/usr"))
        val javaBin  = new File(javaHome, "bin/java").getAbsolutePath

        val cmd: Seq[String] = Seq(javaBin, "-cp", cpString, mainClass) ++ mainArgs

        val exitCode = sys.process.Process(cmd).!(new ProcessLogger {
          def out(s: => String): Unit = log.info(s"  $s")
          def err(s: => String): Unit = log.warn(s"  $s")
          def buffer[T](f: => T): T   = f
        })

        if (exitCode != 0)
          log.error(s"[MermaidDiagramGenerator] MermaidMain exited with code $exitCode")
        else
          log.success(s"[MermaidDiagramGenerator] Diagrams written to ${outDir.getAbsolutePath}")
    }
  }

  override lazy val projectSettings: Seq[Setting[?]] = Seq(
    mermaidOutputDir      := baseDirectory.value / "docs" / "diagrams",
    mermaidGroups         := Map.empty,
    mermaidIncludeObjects := false,
    mermaidMembersMax     := 10,

    generateMermaidDiagram := {
      run(
        log     = streams.value.log,
        outDir  = mermaidOutputDir.value,
        groups  = mermaidGroups.value,
        inclObj = mermaidIncludeObjects.value,
        maxMem  = mermaidMembersMax.value,
        struct  = buildStructure.value,
        refs    = thisProject.value.aggregate,
        sv      = scalaVersion.value,
        fullCp  = (Compile / fullClasspath).value.files,
        st      = state.value
      )
    },

    // Ensure library modules and the mermaid tool are compiled first
    generateMermaidDiagram :=
      (generateMermaidDiagram
        dependsOn (Compile / compile)
        dependsOn (LocalProject("mermaid") / Compile / compile)
        ).value,

    generateMermaidDiagram / aggregate := false
  )
}