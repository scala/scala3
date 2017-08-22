package dotty.tools.sbtplugin

import sbt._
import sbt.Keys._
import sbt.inc.{ ClassfileManager, IncOptions }

object DottyPlugin extends AutoPlugin {
  object autoImport {
    val isDotty = settingKey[Boolean]("Is this project compiled with Dotty?")

    // NOTE:
    // - this is a def to support `scalaVersion := dottyLatestNightlyBuild`
    // - if this was a taskKey, then you couldn't do `scalaVersion := dottyLatestNightlyBuild`
    // - if this was a settingKey, then this would evaluate even if you don't use it.
    def dottyLatestNightlyBuild: Option[String] = {
      println("Fetching latest Dotty nightly version (requires an internet connection)...")
      val Version = """      <version>(0.4\..*-bin.*)</version>""".r
      val latest = scala.io.Source
          .fromURL(
            "http://repo1.maven.org/maven2/ch/epfl/lamp/dotty_0.4/maven-metadata.xml")
          .getLines()
          .collect { case Version(version) => version }
          .toSeq
          .lastOption
      println(s"Latest Dotty nightly build version: $latest")
      latest
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
       *  libraryDependencies += ("a" %% "b" % "c").withDottyCompat()
       *  }}}
       *  This will have no effect when compiling with Scala 2.x, but when compiling
       *  with Dotty this will change the cross-version to a Scala 2.x one. This
       *  works because Dotty is currently retro-compatible with Scala 2.x.
       *
       *  NOTE: Dotty's retro-compatibility with Scala 2.x will be dropped before
       *  Dotty is released, you should not rely on it.
       */
      def withDottyCompat(): ModuleID =
        moduleID.crossVersion match {
          case _: CrossVersion.Binary =>
            moduleID.cross(CrossVersion.binaryMapped { version =>
              CrossVersion.partialVersion(version) match {
                case Some((0, minor)) =>
                  // Dotty v0.4 or greater is compatible with 2.12.x
                  if (minor >= 4) "2.12"
                  else "2.11"
                case _ =>
                  version
              }
            })
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
    val inheritedNewClassfileManager = incOptions.newClassfileManager
    val newClassfileManager = () => new ClassfileManager {
      private[this] val inherited = inheritedNewClassfileManager()

      def delete(classes: Iterable[File]): Unit = {
        val tastySuffixes = List(".tasty", ".hasTasty")
        inherited.delete(classes flatMap { classFile =>
          val dottyFiles = if (classFile.getPath endsWith ".class") {
            val prefix = classFile.getAbsolutePath.stripSuffix(".class")
            tastySuffixes.map(suffix => new File(prefix + suffix)).filter(_.exists)
          } else Nil
          classFile :: dottyFiles
        })
      }

      def generated(classes: Iterable[File]): Unit = inherited.generated(classes)
      def complete(success: Boolean): Unit = inherited.complete(success)
    }
    incOptions.withNewClassfileManager(newClassfileManager)
  }

  override def projectSettings: Seq[Setting[_]] = {
    Seq(
      isDotty := {
        val log = sLog.value

        sbtFullVersion(sbtVersion.value) match {
          case Some((sbtMajor, sbtMinor, sbtPatch)) if sbtMajor == 0 && sbtMinor == 13 && sbtPatch < 15 =>
            log.error(s"The sbt-dotty plugin cannot work with this version of sbt (${sbtVersion.value}), sbt >= 0.13.15 is required.")
            false
          case _ =>
            scalaVersion.value.startsWith("0.")
        }
      },
      scalaOrganization := {
        if (isDotty.value)
          "ch.epfl.lamp"
        else
          scalaOrganization.value
      },

      incOptions in Compile := {
        if (isDotty.value)
          dottyPatchIncOptions((incOptions in Compile).value)
        else
          (incOptions in Compile).value
      },

      scalaBinaryVersion := {
        if (isDotty.value)
          scalaVersion.value.split("\\.").take(2).mkString(".") // Not needed with sbt >= 0.13.16
        else
          scalaBinaryVersion.value
      }
    )
  }
}
