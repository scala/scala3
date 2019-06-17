package dotty.tools.sbtplugin

import sbt._
import sbt.Def.Initialize
import sbt.Keys._
import sbt.librarymanagement.{
  ivy, DependencyResolution, ScalaModuleInfo, SemanticSelector, UpdateConfiguration, UnresolvedWarningConfiguration,
  VersionNumber
}
import sbt.internal.inc.ScalaInstance
import xsbti.compile._
import java.net.URLClassLoader
import java.util.Optional
import scala.util.Properties.isJavaAtLeast

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
        val majorVersionFromWebsite = source0.getLines().toSeq.head
        source0.close()

        // get latest nightly version from maven
        def fetchSource(version: String): (scala.io.BufferedSource, String) =
          try Source.fromURL(s"http://repo1.maven.org/maven2/ch/epfl/lamp/dotty_$version/maven-metadata.xml") -> version
          catch { case t: java.io.FileNotFoundException =>
            val major :: minor :: Nil = version.split('.').toList
            if (minor.toInt <= 0) throw t
            else fetchSource(s"$major.${minor.toInt - 1}")
          }
        val (source1, majorVersion) = fetchSource(majorVersionFromWebsite)
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
       *  NOTE: As a special-case, the cross-version of dotty-library, dotty-compiler and
       *  dotty will never be rewritten because we know that they're Dotty-only.
       *  This makes it possible to do something like:
       *  {{{
       *  libraryDependencies ~= (_.map(_.withDottyCompat(scalaVersion.value)))
       *  }}}
       */
      def withDottyCompat(scalaVersion: String): ModuleID = {
        val name = moduleID.name
        if (name != "dotty" && name != "dotty-library" && name != "dotty-compiler")
          moduleID.crossVersion match {
            case _: librarymanagement.Binary if scalaVersion.startsWith("0.") =>
              moduleID.cross(CrossVersion.constant("2.12"))
            case _ =>
              moduleID
          }
        else
          moduleID
      }
    }
  }

  import autoImport._

  override def requires: Plugins = plugins.JvmPlugin
  override def trigger = allRequirements

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

      val requiredVersion = ">=1.2.7"

      val sbtV = sbtVersion.value
      if (!VersionNumber(sbtV).matchesSemVer(SemanticSelector(requiredVersion)))
        sys.error(s"The sbt-dotty plugin cannot work with this version of sbt ($sbtV), sbt $requiredVersion is required.")

      state
    }
  )

  // https://github.com/sbt/sbt/issues/3110
  val Def = sbt.Def
  override def projectSettings: Seq[Setting[_]] = {
    Seq(
      isDotty := scalaVersion.value.startsWith("0."),

      scalaOrganization := {
        if (isDotty.value)
          "ch.epfl.lamp"
        else
          scalaOrganization.value
      },

      incOptions in Compile := {
        val inc = (incOptions in Compile).value
        if (isDotty.value)
          dottyPatchIncOptions(inc)
        else
          inc
      },

      scalaCompilerBridgeBinaryJar := Def.settingDyn {
        if (isDotty.value) Def.task {
          val updateReport = fetchArtifactsOf(
            scalaOrganization.value % "dotty-sbt-bridge" % scalaVersion.value,
            dependencyResolution.value,
            scalaModuleInfo.value,
            updateConfiguration.value,
            (unresolvedWarningConfiguration in update).value,
            streams.value.log,
          )
          Option(getJar(updateReport, scalaOrganization.value, "dotty-sbt-bridge", scalaVersion.value))
        }
        else Def.task {
          None: Option[File]
        }
      }.value,

      // Needed for RCs publishing
      scalaBinaryVersion := {
        if (isDotty.value)
          scalaVersion.value.split("\\.").take(2).mkString(".")
        else
          scalaBinaryVersion.value
      },

      // Ideally, we should have:
      //
      // 1. Nothing but the Java standard library on the _JVM_ bootclasspath
      //    (starting with Java 9 we cannot inspect it so we don't have a choice)
      //
      // 2. scala-library, dotty-library, dotty-compiler, dotty-doc on the _JVM_
      //    classpath, because we need all of those to actually run the compiler
      //    and the doc tool.
      //    NOTE: All of those should have the *same version* (equal to scalaVersion
      //    for everything but scala-library).
      //
      // 3. scala-library, dotty-library on the _compiler_ bootclasspath because
      //    user code should always have access to the symbols from these jars but
      //    should not be able to shadow them (the compiler bootclasspath has
      //    higher priority than the compiler classpath).
      //    NOTE: the versions of {scala,dotty}-library used here do not necessarily
      //    match the one used in 2. because a dependency of the current project might
      //    require more recent versions, this is OK.
      //
      // 4. every other dependency of the user project on the _compiler_
      //    classpath.
      //
      // Unfortunately, zinc assumes that the compiler bootclasspath is only
      // made of one jar (scala-library), so to make this work we'll need to
      // either change sbt's bootclasspath handling or wait until the
      // dotty-library jar and scala-library jars are merged into one jar.
      // Furthermore, zinc will put on the compiler bootclasspath the
      // scala-library used on the JVM classpath, even if the current project
      // transitively depends on a newer scala-library (this works because Scala
      // 2 guarantees forward- and backward- binary compatibility, but we don't
      // necessarily want to keep doing that in Scala 3).
      // So for the moment, let's just put nothing at all on the compiler
      // bootclasspath, and instead have scala-library and dotty-library on the
      // compiler classpath. This means that user code could shadow symbols
      // from these jars but we can live with that for now.

      // sbt crazy scoping rules mean that when we override `classpathOptions`
      // below we also override `classpathOptions in console` which is normally
      // set in https://github.com/sbt/sbt/blob/b6f02b9b8cd0abb15e3d8856fd76b570deb1bd61/main/src/main/scala/sbt/Defaults.scala#L503,
      // this breaks `sbt console` in Scala 2 projects.
      // There seems to be no way to avoid stomping over task-scoped settings,
      // so we need to manually set `classpathOptions in console` to something sensible,
      // ideally this would be "whatever would be set if this plugin was not enabled",
      // but I can't find a way to do this, so we default to whatever is set in ThisBuild.
      classpathOptions in console := {
        if (isDotty.value)
          classpathOptions.value // The Dotty REPL doesn't require anything special on its classpath
        else
          (classpathOptions in console in ThisBuild).value
      },
      classpathOptions := {
        val old = classpathOptions.value
        if (isDotty.value)
          old
            .withAutoBoot(false)      // we don't put the library on the compiler bootclasspath (as explained above)
            .withFilterLibrary(false) // ...instead, we put it on the compiler classpath
        else
          old
      },
      // ... but when running under Java 8, we still need a compiler bootclasspath
      // that contains the JVM bootclasspath, otherwise sbt incremental
      // compilation breaks.
      scalacOptions ++= {
        if (isDotty.value && !isJavaAtLeast("9"))
          Seq("-bootclasspath", sys.props("sun.boot.class.path"))
        else
          Seq()
      },
      // If the current scalaVersion is N and we transitively depend on
      // {scala, dotty}-{library, compiler, ...} M where M > N, we want the
      // newest version on our compiler classpath, but sbt by default will
      // instead rewrite all our dependencies to version N, the following line
      // prevents this behavior.
      scalaModuleInfo := {
        val old = scalaModuleInfo.value
        if (isDotty.value)
          old.map(_.withOverrideScalaVersion(false))
        else
          old
      },
      // Prevent sbt from creating a ScalaTool configuration
      managedScalaInstance := {
        val old = managedScalaInstance.value
        if (isDotty.value)
          false
        else
          old
      },
      // ... instead, we'll fetch the compiler and its dependencies ourselves.
      scalaInstance := Def.taskDyn {
        if (isDotty.value)
          dottyScalaInstanceTask("dotty-compiler")
        else
          Def.valueStrict { scalaInstance.taskValue }
      }.value,

      // We need more stuff on the classpath to run the `doc` task.
      scalaInstance in doc := Def.taskDyn {
        if (isDotty.value)
          dottyScalaInstanceTask("dotty-doc")
        else
          Def.valueStrict { (scalaInstance in doc).taskValue }
      }.value,

      // Because managedScalaInstance is false, sbt won't add the standard library to our dependencies for us
      libraryDependencies ++= {
        if (isDotty.value && autoScalaLibrary.value)
          Seq(scalaOrganization.value %% "dotty-library" % scalaVersion.value)
        else
          Seq()
      },

      // Turns off the warning:
      // [warn] Binary version (0.9.0-RC1) for dependency ...;0.9.0-RC1
      // [warn]  in ... differs from Scala binary version in project (0.9).
      scalaModuleInfo := {
        val old = scalaModuleInfo.value
        if (isDotty.value)
          old.map(_.withCheckExplicit(false))
        else
          old
      }
    ) ++ inConfig(Compile)(docSettings) ++ inConfig(Test)(docSettings)
  }

  private val docSettings = inTask(doc)(Seq(
    sources := Def.taskDyn {
      val old = sources.value

      if (isDotty.value) Def.task {
        val _ = compile.value // Ensure that everything is compiled, so TASTy is available.
        val tastyFiles = (classDirectory.value ** "*.tasty").get.map(_.getAbsoluteFile)
        old ++ tastyFiles
      } else Def.task {
        old
      }
    }.value,

    scalacOptions ++= {
      if (isDotty.value) {
        val projectName =
          if (configuration.value == Compile)
            name.value
          else
            s"${name.value}-${configuration.value}"
        Seq(
          "-project", projectName,
          "-from-tasty"
        )
      }
      else
        Seq()
    },
  ))

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
  def dottyScalaInstanceTask(moduleName: String): Initialize[Task[ScalaInstance]] = Def.task {
    val updateReport =
      fetchArtifactsOf(
        scalaOrganization.value %% moduleName % scalaVersion.value,
        dependencyResolution.value,
        scalaModuleInfo.value,
        updateConfiguration.value,
        (unresolvedWarningConfiguration in update).value,
        streams.value.log)
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
      allJars
    )
  }

  def makeScalaInstance(
    state: State, dottyVersion: String, scalaLibrary: File, dottyLibrary: File, compiler: File, all: Seq[File]
  ): ScalaInstance = {
    val loader = state.classLoaderCache(all.toList)
    val loaderLibraryOnly = state.classLoaderCache(List(dottyLibrary, scalaLibrary))
    new ScalaInstance(
      dottyVersion,
      loader,
      loaderLibraryOnly,
      scalaLibrary, // Should be a Seq also containing dottyLibrary but zinc
                    // doesn't support this, see comment above our redefinition
                    // of `classpathOption`
      compiler,
      all.toArray,
      None)

  }
}
