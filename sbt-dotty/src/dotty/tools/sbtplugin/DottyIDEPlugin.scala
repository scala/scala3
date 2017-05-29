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
  private[this] def distinctBy[A, B](xs: Seq[A])(f: A => B): Seq[A] = {
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

  private def inAllDottyConfigurations[A](key: TaskKey[A], state: State): Task[Seq[A]] = {
    val struct = Project.structure(state)
    val settings = struct.data
    struct.allProjectRefs.flatMap { projRef =>
      val project = Project.getProjectForReference(projRef, struct).get
      project.configurations.flatMap { config =>
        isDotty.in(projRef, config).get(settings) match {
          case Some(true) =>
            key.in(projRef, config).get(settings)
          case _ =>
            None
        }
      }
    }.join
  }

  private val projectConfig = taskKey[Option[ProjectConfig]]("")
  private val configureIDE = taskKey[Unit]("Generate IDE config files")
  private val compileForIDE = taskKey[Unit]("Compile all projects supported by the IDE")
  private val runCode = taskKey[Unit]("")

  object autoImport {
    val prepareIDE = taskKey[Unit]("Prepare for IDE launch")
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
        val id = s"${thisProject.value.id}/${configuration.value.name}"
        val compilerVersion = scalaVersion.value
          .replace("-nonbootstrapped", "") // The language server is only published bootstrapped
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
    configureIDE := {
      val log = streams.value.log

      val configs0 = state.flatMap(s =>
        inAllDottyConfigurations(projectConfig, s)
      ).value.flatten
      // Drop configurations who do not define their own sources, but just
      // inherit their sources from some other configuration.
      val configs = distinctBy(configs0)(_.sourceDirectories.deep)

      if (configs.isEmpty) {
        log.error("No Dotty project detected")
      } else {
        // If different versions of Dotty are used by subprojects, choose the latest one
        // FIXME: use a proper version number Ordering that knows that "0.1.1-M1" < "0.1.1"
        val ideVersion = configs.map(_.compilerVersion).sorted.last
        // Write the version of the Dotty Language Server to use in a file by itself.
        // This could be a field in the JSON config file, but that would require all
        // IDE plugins to parse JSON.
        val pwArtifact = new PrintWriter(".dotty-ide-artifact")
        pwArtifact.println(s"ch.epfl.lamp:dotty-language-server_0.1:${ideVersion}")
        pwArtifact.close()

        val mapper = new ObjectMapper
        mapper.writerWithDefaultPrettyPrinter()
          .writeValue(new File(".dotty-ide.json"), configs.toArray)
      }
    },

    compileForIDE := {
      val _ = state.flatMap(s =>
        inAllDottyConfigurations(compile, s)
      ).value
    },

    runCode := {
      val exitCode = new ProcessBuilder("code", "--install-extension", "lampepfl.dotty")
        .inheritIO()
        .start()
        .waitFor()
      if (exitCode != 0)
        throw new FeedbackProvidedException {
          override def toString = "Installing the Dotty support for VSCode failed"
        }

      new ProcessBuilder("code", baseDirectory.value.getAbsolutePath)
        .inheritIO()
        .start()
    },

    prepareIDE := {
      val x1 = configureIDE.value
      val x2 = compileForIDE.value
    },

    launchIDE := runCode.dependsOn(prepareIDE).value
  )
}
