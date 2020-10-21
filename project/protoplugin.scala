import sbt._
import sbt.Def.Initialize
import sbt.Keys._
import sbt.librarymanagement.{
  ivy, DependencyResolution, ScalaModuleInfo, SemanticSelector, UpdateConfiguration, UnresolvedWarningConfiguration,
  VersionNumber
}
import sbt.internal.inc.ScalaInstance

import java.net.URLClassLoader

object Protoplugin {
  /** Fetch artifacts for moduleID */
  def fetchArtifactsOf(
    moduleID: ModuleID,
    dependencyRes: DependencyResolution,
    scalaInfo: Option[ScalaModuleInfo],
    updateConfig: UpdateConfiguration,
    warningConfig: UnresolvedWarningConfiguration,
    log: Logger): UpdateReport = {
    val descriptor = dependencyRes.wrapDependencyInModule(moduleID, scalaInfo)

    dependencyRes.update(descriptor, updateConfig, warningConfig, log) match {
      case Right(report) =>
        report
      case _ =>
        throw new MessageOnlyException(
          s"Couldn't retrieve `$moduleID`.")
    }
  }

  /** Get all jars in updateReport that match the given filter. */
  def getJars(updateReport: UpdateReport, organization: NameFilter, name: NameFilter, revision: NameFilter): Seq[File] = {
    updateReport.select(
      configurationFilter(Runtime.name),
      moduleFilter(organization, name, revision),
      artifactFilter(extension = "jar", classifier = "")
    )
  }

  /** Get the single jar in updateReport that match the given filter.
    *  If zero or more than one jar match, an exception will be thrown.
    */
  def getJar(updateReport: UpdateReport, organization: NameFilter, name: NameFilter, revision: NameFilter): File = {
    val jars = getJars(updateReport, organization, name, revision)
    assert(jars.size == 1, s"There should only be one $name jar but found: $jars")
    jars.head
  }

  /** Create a scalaInstance task that uses Dotty based on `moduleName`. */
  def dottyScalaInstanceTask(module: ModuleID): Initialize[Task[ScalaInstance]] = Def.task {
    val updateReport =
      fetchArtifactsOf(
        module,
        dependencyResolution.value,
        scalaModuleInfo.value,
        updateConfiguration.value,
        (unresolvedWarningConfiguration in update).value,
        streams.value.log
      )
    val scalaLibraryJar = getJar(updateReport,
      "org.scala-lang", "scala-library", revision = AllPassFilter)
    val dottyLibraryJar = getJar(updateReport,
      scalaOrganization.value, s"dotty-library_${scalaBinaryVersion.value}", scalaVersion.value)
    val compilerJar = getJar(updateReport,
      scalaOrganization.value, s"dotty-compiler_${scalaBinaryVersion.value}", scalaVersion.value)
    val allJars =
      getJars(updateReport, AllPassFilter, AllPassFilter, AllPassFilter)

    makeScalaInstance(
      state.value,
      scalaVersion.value,
      scalaLibraryJar,
      dottyLibraryJar,
      compilerJar,
      allJars,
    )
  }

  // Adapted from private mkScalaInstance in sbt
  def makeScalaInstance(
    state: State, dottyVersion: String, scalaLibrary: File, dottyLibrary: File, compiler: File, all: Seq[File]
  ): ScalaInstance = {
    val libraryLoader = state.classLoaderCache(List(dottyLibrary, scalaLibrary))
    class DottyLoader
        extends URLClassLoader(all.map(_.toURI.toURL).toArray, libraryLoader)
    val fullLoader = state.classLoaderCache.cachedCustomClassloader(
      all.toList,
      () => new DottyLoader
    )
    new ScalaInstance(
      dottyVersion,
      fullLoader,
      libraryLoader,
      Array(dottyLibrary, scalaLibrary),
      compiler,
      all.toArray,
      None)

  }


  object Keys {
    lazy val scala3docOptions = settingKey[Seq[String]]("Options for Scala3doc")
    lazy val serveDoc = taskKey[Unit]("Start a local HTTP server for the documentation")
    lazy val serveDocPort = settingKey[String]("The port on which serveDoc should run an HTTP server")
    lazy val serveDocPrimaryExecutable = settingKey[String]("The primary Python executable serveDoc should use")
    lazy val serveDocSecondaryExecutable = settingKey[String]("The secondary Python executable serveDoc should use")
  }
  import Keys._

  lazy val Settings = Seq(
    resolvers += Resolver.jcenterRepo,
    resolvers += Resolver.mavenLocal, // necessary for Scala3doc Kotlin code
    resolvers += Resolver.bintrayRepo("kotlin", "kotlin-dev"),
    resolvers += Resolver.bintrayRepo("virtuslab", "dokka"),
    scala3docOptions := Seq.empty,
    Compile / doc / scalacOptions := {
      scala3docOptions.value.map("--+DOC+" + _) ++ (Compile / doc / scalacOptions).value
    },
    scalaInstance in doc := {
      dottyScalaInstanceTask(
        "scala3doc" %% "scala3doc" % "0.1.1-SNAPSHOT",
      ).value
    },

    serveDocPort := "8000",
    serveDocPrimaryExecutable := "python3",
    serveDocSecondaryExecutable := "python",
    serveDoc / target := (Compile / doc / target).value,
    serveDoc := {
      import scala.sys.process._
      val log = streams.value.log
      val port = serveDocPort.value
      val _target = (serveDoc / target).value
      def go(exec: String) = {
        log.info(s"Running a $exec HTTP server at http://localhost:$port in ${_target}")
        log.info("Press Ctrl-c to interrupt...")
        Process(
          Seq(exec, "-m", "http.server", port),
          _target,
        ).!
      }

      val exec = serveDocPrimaryExecutable.value
      try go(exec)
      catch {
        case ex: java.io.IOException =>
          if (ex.getMessage.contains("No such file or directory")) {
            val altExec = serveDocSecondaryExecutable.value
            log.warn(s"Could not find `$exec` executable, retrying with `$altExec` ...")
            go(altExec)
          }
          else throw ex
      }
    },
  )
}
