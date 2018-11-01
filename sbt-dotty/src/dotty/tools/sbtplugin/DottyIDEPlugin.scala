package dotty.tools.sbtplugin

import sbt._
import sbt.Def.Initialize
import sbt.Keys._
import java.io._
import java.lang.ProcessBuilder
import java.lang.ProcessBuilder.Redirect
import scala.collection.mutable
import scala.util.Properties.{ isWin, isMac }

import dotty.tools.languageserver.config.ProjectConfig

import com.fasterxml.jackson.databind.ObjectMapper
import scala.collection.mutable.ListBuffer
import DottyPlugin.autoImport._

object DottyIDEPlugin extends AutoPlugin {
  object autoImport {
    val excludeFromIDE = settingKey[Boolean]("If true, do not import this project or configuration in the Dotty IDE")

    val codeCommand = taskKey[Seq[String]]("Command to start VSCode")
    val runCode = taskKey[Unit]("Start VSCode, usually called from launchIDE")
    val launchIDE = taskKey[Unit]("Configure and run VSCode on this project")
  }

  import autoImport._

  // Adapted from scala-reflect
  private def distinctBy[A, B](xs: Seq[A])(f: A => B): Seq[A] = {
    val buf = new mutable.ListBuffer[A]
    val seen = mutable.Set[B]()
    xs foreach { x =>
      val y = f(x)
      if (!seen(y)) {
        buf += x
        seen += y
      }
    }
    buf.toList
  }

  private def isDottyVersion(version: String) =
    version.startsWith("0.")


  /** Return a new state derived from `state` such that scalaVersion returns `newScalaVersion` in all
   *  projects in `projRefs` (`state` is returned if no setting needed to be updated).
   */
  private def updateScalaVersion(state: State, projRefs: Seq[ProjectRef], newScalaVersion: String): State = {
    val extracted = Project.extract(state)
    val settings = extracted.structure.data

    if (projRefs.forall(projRef => scalaVersion.in(projRef).get(settings).get == newScalaVersion))
      state
    else {
      def matchingSetting(setting: Setting[_]) =
        setting.key.key == scalaVersion.key &&
        setting.key.scope.project.fold(ref => projRefs.contains(ref), ifZero = true, ifThis = true)

      val newSettings = extracted.session.mergeSettings.collect {
        case setting if matchingSetting(setting) =>
          scalaVersion in setting.key.scope := newScalaVersion
      }
      val newSession = extracted.session.appendRaw(newSettings)
      BuiltinCommands.reapply(newSession, extracted.structure, state)
    }
  }

  /** Setup to run in all dotty projects.
   *  Return a triplet of:
   *  (1) A version of dotty
   *  (2) A list of dotty projects
   *  (3) A state where `scalaVersion` is set to (1) in all projects in (2)
   */
  private def dottySetup(state: State): (String, Seq[ProjectRef], State) = {
    val structure = Project.structure(state)
    val settings = structure.data

    // FIXME: this function uses `sorted` to order versions but this is incorrect,
    // we need an Ordering for version numbers, like the one in Coursier.

    val (dottyVersions, dottyProjRefs) =
      structure.allProjectRefs.flatMap { projRef =>
        val version = scalaVersion.in(projRef).get(settings).get
        if (isDottyVersion(version))
          Some((version, projRef))
        else
          crossScalaVersions.in(projRef).get(settings).get.filter(isDottyVersion).sorted.lastOption match {
            case Some(v) =>
              Some((v, projRef))
            case _ =>
              None
          }
      }.unzip

    if (dottyVersions.isEmpty)
      throw new MessageOnlyException("No Dotty project detected")
    else {
      val dottyVersion = dottyVersions.sorted.last
      val dottyState = updateScalaVersion(state, dottyProjRefs, dottyVersion)
      (dottyVersion, dottyProjRefs, dottyState)
    }
  }

  /** Run `task` in state `state` */
  private def runTask[T](task: Task[T], state: State): T = {
    val extracted = Project.extract(state)
    val structure = extracted.structure
    val (_, result) =
      EvaluateTask.withStreams(structure, state) { streams =>
        EvaluateTask.runTask(task, state, streams, structure.index.triggers,
          EvaluateTask.extractedTaskConfig(extracted, structure, state))(
          EvaluateTask.nodeView(state, streams, Nil)
        )
      }
    result match {
      case Value(v) =>
        v
      case Inc(i) =>
        throw i
    }
  }

  /** Run task `key` in all configurations in all projects in `projRefs`, using state `state`,
   *  configurations where `excludeFromIDE` is `true` are skipped. */
  private def runInAllIDEConfigurations[T](key: TaskKey[T], projRefs: Seq[ProjectRef], state: State): Seq[T] = {
    val structure = Project.structure(state)
    val settings = structure.data
    val joinedTask = projRefs.flatMap { projRef =>
      val project = Project.getProjectForReference(projRef, structure).get
      project.configurations.flatMap { config =>
        excludeFromIDE.in(projRef, config).get(settings) match {
          case Some(true) =>
            None // skip this configuration
          case _ =>
            key.in(projRef, config).get(settings)
        }
      }
    }.join

    runTask(joinedTask, state)
  }

  /** Prepare command to be passed to ProcessBuilder */
  def prepareCommand(cmd: Seq[String]): Seq[String] =
    if (isWin) Seq("cmd.exe", "/C") ++ cmd
    else cmd

  /** Run the command `cmd`.
   *
   *  @param wait  If true, wait for the command to return and throw an exception if the exit code is non-zero.
   *  @param directory  If not None, run the command in this directory.
   *  @param outputCallback If not None, pass the command output to this callback instead of writing it to stdout.
   */
  def runProcess(cmd: Seq[String], wait: Boolean = false, directory: Option[File] = None, outputCallback: Option[BufferedReader => Unit] = None): Unit = {
    val pb = new ProcessBuilder(prepareCommand(cmd): _*)

    directory match {
      case Some(dir) =>
        pb.directory(dir)
      case None =>
    }

    pb.redirectInput(Redirect.INHERIT)
      .redirectError(Redirect.INHERIT)
      .redirectOutput(
        outputCallback match {
          case Some(_) =>
            Redirect.PIPE
          case None =>
            Redirect.INHERIT
        })

    val process = pb.start()
    outputCallback match {
      case Some(callback) =>
        callback(new BufferedReader(new InputStreamReader(process.getInputStream)))
      case None =>
    }
    if (wait) {
      val exitCode = process.waitFor()
      if (exitCode != 0) {
        val cmdString = cmd.mkString(" ")
        val description = if (directory != null) s""" in directory "$directory"""" else ""
        throw new MessageOnlyException(s"""Running command "${cmdString}"${description} failed.""")
      }
    }
  }

  /** Install or upgrade Code extension `name`.
   *
   *  We start by trying to install or upgrade the extension. If this fails we
   *  check if an existing version of the extension exists. If this also fails
   *  we throw an exception. This ensures that we're always running the latest
   *  version of the extension but that we can still work offline.
   */
  def installCodeExtension(codeCmd: Seq[String], name: String): Unit = {
    try {
      runProcess(codeCmd ++ Seq("--force", "--install-extension", name), wait = true)
    } catch {
      case e: Exception =>
        var alreadyInstalled: Boolean = false
        runProcess(codeCmd ++ Seq("--list-extensions"), wait = true, outputCallback = Some({ br =>
          alreadyInstalled = br.lines.filter(_ == name).findFirst.isPresent
        }))
        if (!alreadyInstalled)
          throw e
    }
  }

  private val projectConfig = taskKey[Option[ProjectConfig]]("")

  override def requires: Plugins = plugins.JvmPlugin
  override def trigger = allRequirements

  private val artifactFile = new File(".dotty-ide-artifact")
  private val configFile = new File(".dotty-ide.json")

  def configureIDE = Command.command("configureIDE") { origState =>
    val (dottyVersion, projRefs, dottyState) = dottySetup(origState)
    val configs0 = runInAllIDEConfigurations(projectConfig, projRefs, dottyState).flatten

    // Drop configurations that do not define their own sources, but just
    // inherit their sources from some other configuration.
    val configs = distinctBy(configs0)(_.sourceDirectories.deep)

    // Write the version of the Dotty Language Server to use in a file by itself.
    // This could be a field in the JSON config file, but that would require all
    // IDE plugins to parse JSON.
    val dlsVersion = dottyVersion
      .replace("-nonbootstrapped", "") // The language server is only published bootstrapped
    val dlsBinaryVersion = dlsVersion.split("\\.").take(2).mkString(".")
    val pwArtifact = new PrintWriter(artifactFile)
    try {
      pwArtifact.println(s"ch.epfl.lamp:dotty-language-server_${dlsBinaryVersion}:${dlsVersion}")
    } finally {
      pwArtifact.close()
    }

    val mapper = new ObjectMapper
    mapper.writerWithDefaultPrettyPrinter()
      .writeValue(configFile, configs.toArray)

    origState
  }

  def compileForIDE = Command.command("compileForIDE") { origState =>
    val (dottyVersion, projRefs, dottyState) = dottySetup(origState)
    runInAllIDEConfigurations(compile, projRefs, dottyState)

    origState
  }

  def launchIDE = Command.command("launchIDE") { state0 =>
    val state1 = try {
      Command.process("configureIDE", state0)
    } catch {
      case i: Incomplete =>
        if (artifactFile.exists && configFile.exists) {
          state0.log.error("IDE configuration failed, launching the IDE using the previous configuration")
          state0: State
        } else {
          state0.log.error("IDE configuration failed and no previous configuration found")
          state0.log.error("Please fix the compilation errors then run 'launchIDE' again")
          throw i
        }
    }
    Command.process("runCode", state1)
  }

  private def projectConfigTask(config: Configuration): Initialize[Task[Option[ProjectConfig]]] = Def.taskDyn {
    val depClasspath = Attributed.data((dependencyClasspath in config).value)

    // Try to detect if this is a real Scala project or not. This is pretty
    // fragile because sbt simply does not keep track of this information. We
    // could check if at least one source file ends with ".scala" but that
    // doesn't work for empty projects.
    val isScalaProject = depClasspath.exists(_.getAbsolutePath.contains("dotty-library")) && depClasspath.exists(_.getAbsolutePath.contains("scala-library"))

    if (!isScalaProject) Def.task { None }
    else Def.task {
      // Not needed to generate the config, but this guarantees that the
      // generated config is usable by an IDE without any extra compilation
      // step.
      val _ = (compile in config).value

      val id = s"${thisProject.value.id}/${config.name}"
      val compilerVersion = (scalaVersion in config).value
      val compilerArguments = (scalacOptions in config).value
      val sourceDirectories = (unmanagedSourceDirectories in config).value ++ (managedSourceDirectories in config).value
      val classDir = (classDirectory in config).value

      Some(new ProjectConfig(
        id,
        compilerVersion,
        compilerArguments.toArray,
        sourceDirectories.toArray,
        depClasspath.toArray,
        classDir
      ))
    }
  }

  override def projectSettings: Seq[Setting[_]] = Seq(
    // TODO: It would be better to use Def.derive to define projectConfig in
    // every configuration where the keys it depends on exist, however this
    // currently breaks aggregated tasks: https://github.com/sbt/sbt/issues/3580
    projectConfig in Compile := projectConfigTask(Compile).value,
    projectConfig in Test := projectConfigTask(Test).value
  )

  override def buildSettings: Seq[Setting[_]] = Seq(
    commands ++= Seq(configureIDE, compileForIDE, launchIDE),

    excludeFromIDE := false,

    codeCommand := {
      Seq("code", "-n")
    },

    runCode := {
      try {
        installCodeExtension(codeCommand.value, "lampepfl.dotty")

        runProcess(codeCommand.value ++ Seq("."), directory = Some(baseDirectory.value))
      } catch {
        case ioex: IOException if ioex.getMessage.startsWith("""Cannot run program "code"""") =>
          val log = streams.value.log
          log.error(
            """Could not find Visual Studio Code on your system.
              |Follow the instructions at http://dotty.epfl.ch/docs/usage/ide-support.html
              |to install it.""".stripMargin)
          throw new FeedbackProvidedException {
            override def toString = "Could not find Visual Studio Code on your system."
          }
      }
    }

  ) ++ addCommandAlias("launchIDE", ";configureIDE;runCode")
}
