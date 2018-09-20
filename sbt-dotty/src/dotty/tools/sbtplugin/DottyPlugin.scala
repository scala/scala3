package dotty.tools.sbtplugin

import sbt._
import sbt.Keys._
import sbt.librarymanagement.DependencyResolution
import sbt.internal.inc.ScalaInstance
import xsbti.compile._
import java.net.URLClassLoader
import java.util.Optional

object DottyPlugin extends AutoPlugin {
  object autoImport {
    val isDotty = settingKey[Boolean]("Is this project compiled with Dotty?")

    // NOTE:
    // - this is a def to support `scalaVersion := dottyLatestNightlyBuild`
    // - if this was a taskKey, then you couldn't do `scalaVersion := dottyLatestNightlyBuild`
    // - if this was a settingKey, then this would evaluate even if you don't use it.
    def dottyLatestNightlyBuild(): Option[String] = {
      import scala.io.Source

      println("Fetching latest Dotty nightly version...")

      val nightly = try {
        // get majorVersion from dotty.epfl.ch
        val source0 = Source.fromURL("http://dotty.epfl.ch/versions/latest-nightly-base")
        val majorVersion = source0.getLines().toSeq.head
        source0.close()

        // get latest nightly version from maven
        val source1 =
          Source.fromURL(s"http://repo1.maven.org/maven2/ch/epfl/lamp/dotty_$majorVersion/maven-metadata.xml")
        val Version = s"      <version>($majorVersion\\..*-bin.*)</version>".r
        val nightly = source1
          .getLines()
          .collect { case Version(version) => version }
          .toSeq
          .lastOption
        source1.close()
        nightly
      } catch {
        case _:java.net.UnknownHostException =>
          None
      }

      nightly match {
        case Some(version) =>
          println(s"Latest Dotty nightly build version: $version")
        case None =>
          println(s"Unable to get Dotty latest nightly build version. Make sure you are connected to internet")
      }

      nightly
    }

    implicit class DottyCompatModuleID(moduleID: ModuleID) {
      /** If this ModuleID cross-version is a Dotty version, replace it
       *  by the Scala 2.x version that the Dotty version is retro-compatible with,
       *  otherwise do nothing.
       *
       *  This setting is useful when your build contains dependencies that have only
       *  been published with Scala 2.x, if you have:
       *  {{{
       *  libraryDependencies += "a" %% "b" % "c"
       *  }}}
       *  you can replace it by:
       *  {{{
       *  libraryDependencies += ("a" %% "b" % "c").withDottyCompat(scalaVersion.value)
       *  }}}
       *  This will have no effect when compiling with Scala 2.x, but when compiling
       *  with Dotty this will change the cross-version to a Scala 2.x one. This
       *  works because Dotty is currently retro-compatible with Scala 2.x.
       *
       *  NOTE: Dotty's retro-compatibility with Scala 2.x will be dropped before
       *  Dotty is released, you should not rely on it.
       */
      def withDottyCompat(scalaVersion: String): ModuleID =
      moduleID.crossVersion match {
          case _: librarymanagement.Binary if scalaVersion.startsWith("0.") =>
            moduleID.cross(CrossVersion.constant("2.12"))
          case _ =>
            moduleID
        }
    }
  }

  import autoImport._

  override def requires: Plugins = plugins.JvmPlugin
  override def trigger = allRequirements

  // Adapted from CrossVersionUtil#sbtApiVersion
  private def sbtFullVersion(v: String): Option[(Int, Int, Int)] =
  {
    val ReleaseV = """(\d+)\.(\d+)\.(\d+)(-\d+)?""".r
    val CandidateV = """(\d+)\.(\d+)\.(\d+)(-RC\d+)""".r
    val NonReleaseV = """(\d+)\.(\d+)\.(\d+)([-\w+]*)""".r
    v match {
      case ReleaseV(x, y, z, ht) => Some((x.toInt, y.toInt, z.toInt))
      case CandidateV(x, y, z, ht)  => Some((x.toInt, y.toInt, z.toInt))
      case NonReleaseV(x, y, z, ht) if z.toInt > 0 => Some((x.toInt, y.toInt, z.toInt))
      case _ => None
    }
  }

  /** Patches the IncOptions so that .tasty and .hasTasty files are pruned as needed.
   *
   *  This code is adapted from `scalaJSPatchIncOptions` in Scala.js, which needs
   *  to do the exact same thing but for classfiles.
   *
   *  This complicated logic patches the ClassfileManager factory of the given
   *  IncOptions with one that is aware of .tasty and .hasTasty files emitted by the Dotty
   *  compiler. This makes sure that, when a .class file must be deleted, the
   *  corresponding .tasty or .hasTasty file is also deleted.
   */
  def dottyPatchIncOptions(incOptions: IncOptions): IncOptions = {
    val tastyFileManager = new TastyFileManager

    // Once sbt/zinc#562 is fixed, can be:
    // val newExternalHooks =
    //   incOptions.externalHooks.withExternalClassFileManager(tastyFileManager)
    val inheritedHooks = incOptions.externalHooks
    val external = Optional.of(tastyFileManager: ClassFileManager)
    val prevManager = inheritedHooks.getExternalClassFileManager
    val fileManager: Optional[ClassFileManager] =
      if (prevManager.isPresent) Optional.of(WrappedClassFileManager.of(prevManager.get, external))
      else external
    val newExternalHooks = new DefaultExternalHooks(inheritedHooks.getExternalLookup, fileManager)

    incOptions.withExternalHooks(newExternalHooks)
  }

  override val globalSettings: Seq[Def.Setting[_]] = Seq(
    onLoad in Global := onLoad.in(Global).value.andThen { state =>
      val sbtV = sbtVersion.value
      sbtFullVersion(sbtV) match {
        case Some((1, sbtMinor, sbtPatch)) if sbtMinor > 1 || (sbtMinor == 1  && sbtPatch >= 5) =>
        case _ =>
          sys.error(s"The sbt-dotty plugin cannot work with this version of sbt ($sbtV), sbt >= 1.1.5 is required.")
      }
      state
    }
  )

  override def projectSettings: Seq[Setting[_]] = {
    Seq(
      isDotty := scalaVersion.value.startsWith("0."),

      scalaOrganization := {
        if (isDotty.value)
          "ch.epfl.lamp"
        else
          scalaOrganization.value
      },

      scalacOptions in (Compile, doc) ++= Seq("-project", name.value),

      incOptions in Compile := {
        val inc = (incOptions in Compile).value
        if (isDotty.value)
          dottyPatchIncOptions(inc)
        else
          inc
      },

      scalaCompilerBridgeSource := {
        val scalaBridge = scalaCompilerBridgeSource.value
        val dottyBridge = (scalaOrganization.value % "dotty-sbt-bridge" % scalaVersion.value).withConfigurations(Some(Configurations.Compile.name)).sources()
        if (isDotty.value)
          dottyBridge
        else
          scalaBridge
      },

      // Needed for RCs publishing
      scalaBinaryVersion := {
        if (isDotty.value)
          scalaVersion.value.split("\\.").take(2).mkString(".")
        else
          scalaBinaryVersion.value
      },

      scalaInstance := Def.taskDyn {
        val si = scalaInstance.value
        if (isDotty.value) {
          Def.task {
            val dottydocArtifacts = fetchArtifactsOf("dotty-doc").value
            val includeArtifact = (f: File) => f.getName.endsWith(".jar")
            val dottydocJars = dottydocArtifacts.filter(includeArtifact).toArray
            val allJars = (si.allJars ++ dottydocJars).distinct
            val loader = new URLClassLoader(Path.toURLs(dottydocJars), si.loader)
            new ScalaInstance(si.version, loader, si.loaderLibraryOnly, si.libraryJar, si.compilerJar, allJars, si.explicitActual)
          }
        } else {
          Def.task { si }
        }
      }.value,

      scalaModuleInfo := {
        val old = scalaModuleInfo.value
        if (isDotty.value) {
          // Turns off the warning:
          // [warn] Binary version (0.9.0-RC1) for dependency ...;0.9.0-RC1
          // [warn]  in ... differs from Scala binary version in project (0.9).
          old.map(_.withCheckExplicit(false))
        } else old
      },

      updateOptions := {
        val old = updateOptions.value
        if (isDotty.value) {
          // Turn off the warning:
          //  circular dependency found:
          //    ch.epfl.lamp#scala-library;0.9.0-RC1->ch.epfl.lamp#dotty-library_0.9;0.9.0-RC1->...
          // (This should go away once we merge dotty-library and scala-library in one artefact)
          old.withCircularDependencyLevel(sbt.librarymanagement.ivy.CircularDependencyLevel.Ignore)
        } else old
      }
    )
  }

  /** Fetch artefacts for scalaOrganization.value %% moduleName % scalaVersion.value */
  private def fetchArtifactsOf(moduleName: String) = Def.task {
    val dependencyResolution = Keys.dependencyResolution.value
    val log = streams.value.log
    val scalaInfo = scalaModuleInfo.value
    val updateConfiguration = Keys.updateConfiguration.value
    val warningConfiguration = (unresolvedWarningConfiguration in update).value

    val moduleID = (scalaOrganization.value %% moduleName % scalaVersion.value).cross(CrossVersion.binary)
    val descriptor = dependencyResolution.wrapDependencyInModule(moduleID, scalaInfo)

    dependencyResolution.update(descriptor, updateConfiguration, warningConfiguration, log) match {
      case Right(report) =>
        report.allFiles
      case _ =>
        throw new MessageOnlyException(
          s"Couldn't retrieve `${scalaOrganization.value} %% $moduleName %% ${scalaVersion.value}`.")
    }
  }
}
