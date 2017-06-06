package dotty.tools.sbtplugin

import sbt._
import sbt.Keys._
import java.io._
import java.lang.ProcessBuilder
import scala.collection.mutable

import dotty.tools.languageserver.config.ProjectConfig

import com.fasterxml.jackson.databind.ObjectMapper
import scala.collection.mutable.ListBuffer
import DottyPlugin.autoImport._

object DottyIDEPlugin extends AutoPlugin {
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
        setting.key.scope.project.fold(ref => projRefs.contains(ref), ifGlobal = true, ifThis = true)

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

  /** Run task `key` in all configurations in all projects in `projRefs`, using state `state` */
  private def runInAllConfigurations[T](key: TaskKey[T], projRefs: Seq[ProjectRef], state: State): Seq[T] = {
    val structure = Project.structure(state)
    val settings = structure.data
    val joinedTask = projRefs.flatMap { projRef =>
      val project = Project.getProjectForReference(projRef, structure).get
      project.configurations.flatMap { config =>
        key.in(projRef, config).get(settings)
      }
    }.join

    runTask(joinedTask, state)
  }

  private val projectConfig = taskKey[Option[ProjectConfig]]("")
  private val compileForIDE = taskKey[Unit]("Compile all projects supported by the IDE")
  private val runCode = taskKey[Unit]("")

  object autoImport {
    val configureIDE = taskKey[Unit]("Generate IDE config files")
    val launchIDE = taskKey[Unit]("Run Visual Studio Code on this project")
  }

  import autoImport._

  override def requires: Plugins = plugins.JvmPlugin
  override def trigger = allRequirements


  override def projectSettings: Seq[Setting[_]] = Seq(
    // Use Def.derive so `projectConfig` is only defined in the configurations where the
    // tasks/settings it depends on are defined.
    Def.derive(projectConfig := {
      if (sources.value.isEmpty) None
      else {
        // Not needed to generate the config, but this guarantees that the
        // generated config is usable by an IDE without any extra compilation
        // step.
        val _ = compile.value

        val id = s"${thisProject.value.id}/${configuration.value.name}"
        val compilerVersion = scalaVersion.value
        val compilerArguments = scalacOptions.value
        val sourceDirectories = unmanagedSourceDirectories.value ++ managedSourceDirectories.value
        val depClasspath = Attributed.data(dependencyClasspath.value)
        val classDir = classDirectory.value

        Some(new ProjectConfig(
          id,
          compilerVersion,
          compilerArguments.toArray,
          sourceDirectories.toArray,
          depClasspath.toArray,
          classDir
        ))
      }
    })
  )

  override def buildSettings: Seq[Setting[_]] = Seq(
    configureIDE := Def.taskDyn {
      val origState = state.value
      Def.task {
        val (dottyVersion, projRefs, dottyState) = dottySetup(origState)
        val configs0 = runInAllConfigurations(projectConfig, projRefs, dottyState).flatten

        // Drop configurations that do not define their own sources, but just
        // inherit their sources from some other configuration.
        val configs = distinctBy(configs0)(_.sourceDirectories.deep)

        // Write the version of the Dotty Language Server to use in a file by itself.
        // This could be a field in the JSON config file, but that would require all
        // IDE plugins to parse JSON.
        val dlsVersion = dottyVersion
          .replace("-nonbootstrapped", "") // The language server is only published bootstrapped
        val dlsBinaryVersion = dlsVersion.split("\\.").take(2).mkString(".")
        val pwArtifact = new PrintWriter(".dotty-ide-artifact")
        pwArtifact.println(s"ch.epfl.lamp:dotty-language-server_${dlsBinaryVersion}:${dlsVersion}")
        pwArtifact.close()

        val mapper = new ObjectMapper
        mapper.writerWithDefaultPrettyPrinter()
          .writeValue(new File(".dotty-ide.json"), configs.toArray)
      }
    }.value,

    compileForIDE := Def.taskDyn {
      val origState = state.value
      Def.task {
        val (dottyVersion, projRefs, dottyState) = dottySetup(origState)
        runInAllConfigurations(compile, projRefs, dottyState)
        ()
      }
    }.value,

    runCode := {
      val exitCode = new ProcessBuilder("code", "--install-extension", "lampepfl.dotty")
        .inheritIO()
        .start()
        .waitFor()
      if (exitCode != 0)
        throw new MessageOnlyException("Installing the Dotty support for VSCode failed")

      new ProcessBuilder("code", baseDirectory.value.getAbsolutePath)
        .inheritIO()
        .start()
    },

    launchIDE := runCode.dependsOn(configureIDE).value
  )
}
