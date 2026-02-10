package scala3build

object Versions {

  /** Version of the Scala compiler used to build the artifacts.
   *  Reference version should track the latest version pushed to Maven:
   *  - In main branch it should be the last RC version
   *  - In release branch it should be the last stable release
   *
   *  Warning: Change of this variable needs to be consulted with `expectedTastyVersion`
   */
  val referenceVersion = "3.8.2-RC1"

  /** Version of the Scala compiler targeted in the current release cycle
   *  Contains a version without RC/SNAPSHOT/NIGHTLY specific suffixes
   *  Should be updated ONLY after release or cutoff for previous release cycle.
   *
   *  Should only be referred from `dottyVersion` or settings/tasks requiring simplified version string,
   *  eg. `compatMode` or Windows native distribution version.
   *
   *  Warning: Change of this variable might require updating `expectedTastyVersion`
   */
  val developedVersion = "3.8.3"

  /** The version of the compiler including the RC prefix.
   *  Defined as common base before calculating environment specific suffixes in `dottyVersion`
   *
   *  By default, during development cycle defined as `${developedVersion}-RC1`;
   *  During release candidate cycle incremented by the release officer before publishing a subsequent RC version;
   *  During final, stable release is set exactly to `developedVersion`.
  */
  val baseVersion = s"$developedVersion-RC1"

  /** The version of TASTY that should be emitted, checked in runtime test
   *  For defails on how TASTY version should be set see related discussions:
   *    - https://github.com/scala/scala3/issues/13447#issuecomment-912447107
   *    - https://github.com/scala/scala3/issues/14306#issuecomment-1069333516
   *    - https://github.com/scala/scala3/pull/19321
   *
   *  Simplified rules, given 3.$minor.$patch = $developedVersion
   *    - Major version is always 28
   *    - TASTY minor version:
   *      - in main (NIGHTLY): {if $patch == 0 || ${referenceVersion.matches(raw"3.$minor.0-RC\d")} then $minor else ${minor + 1}}
   *      - in release branch is always equal to $minor
   *    - TASTY experimental version:
   *      - in main (NIGHTLY) is always experimental
   *      - in release candidate branch is experimental if {patch == 0}
   *      - in stable release is always non-experimetnal
   */
  val expectedTastyVersion = "28.9-experimental-1"
  checkReleasedTastyVersion()

  /** Final version of Scala compiler, controlled by environment variables. */
  val dottyVersion = {
    if (isRelease) baseVersion
    else if (isNightly) {
      val formatter = java.time.format.DateTimeFormatter.ofPattern("yyyyMMdd")
      val currentDate =
        formatter.format(java.time.ZonedDateTime.now(java.time.ZoneId.of("UTC")))
      s"${baseVersion}-bin-${currentDate}-${VersionUtil.gitHash}-NIGHTLY"
    }
    else s"${baseVersion}-bin-SNAPSHOT"
  }
  def isRelease = sys.env.get("RELEASEBUILD").contains("yes")
  def isNightly = sys.env.get("NIGHTLYBUILD").contains("yes")

  /** Version calculate for `nonbootstrapped` projects */
  val dottyNonBootstrappedVersion = {
    // Make sure sbt always computes the scalaBinaryVersion correctly
    val bin = if (!dottyVersion.contains("-bin")) "-bin" else ""
    dottyVersion + bin + "-nonbootstrapped"
  }

  // LTS or Next
  val versionLine = "Next"

  // Versions used by the vscode extension to create a new project
  // This should be the latest published releases.
  // TODO: Have the vscode extension fetch these numbers from the Internet
  // instead of hardcoding them ?
  val publishedDottyVersion = referenceVersion
  val sbtDottyVersion = "0.5.5"

  /** Minor version against which we check binary compatibility.
   *
   *  This must be the earliest published release in the same versioning line.
   *  For a developedVersion `3.M.P` the mimaPreviousDottyVersion should be set to:
   *   - `3.M.0`     if `P > 0`
   *   - `3.(M-1).0` if `P = 0`
   */
  val mimaPreviousDottyVersion = "3.7.3" // for 3.8.0, we compare against 3.7.3

  /** LTS version against which we check binary compatibility.
   *
   *  This must be the earliest published release in the LTS versioning line.
   *  For example, if the latest LTS release is be 3.3.4, then this must be
   *  set to 3.3.0.
   */
  val mimaPreviousLTSDottyVersion = "3.3.0"

  /** Version of the scala-library for which we will generate TASTy.
   *
   *  We should never use a nightly version here to release.
   *
   *  We can use nightly versions to tests the future compatibility in development.
   *  Nightly versions: https://scala-ci.typesafe.com/ui/native/scala-integration/org/scala-lang
   */
  val stdlibBootstrappedVersion = "2.13.16"

  /** Version of the compatible Scala 2.13 scala-library
   *  Should be updated when we synchronize with the sources of Scala 2.
   *
   *  This version would be used to fetch sources of Scala 2.13 standard library to be used for patching the Scala 3 standard library.
   */
  val scala2Version      = "2.13.18"

  /* Tests TASTy version invariants during NIGHLY, RC or Stable releases */
  private def checkReleasedTastyVersion(): Unit = {
    case class ScalaVersion(minor: Int, patch: Int, isRC: Boolean)
    def parseScalaVersion(version: String): ScalaVersion = version.split("\\.|-").take(4) match {
      case Array("3", minor, patch)    => ScalaVersion(minor.toInt, patch.toInt, false)
      case Array("3", minor, patch, _) => ScalaVersion(minor.toInt, patch.toInt, true)
      case other => sys.error(s"Invalid Scala base version string: $baseVersion")
    }
    lazy val version = parseScalaVersion(baseVersion)
    lazy val referenceV = parseScalaVersion(referenceVersion)
    lazy val (tastyMinor, tastyIsExperimental) = expectedTastyVersion.split("\\.|-").take(4) match {
      case Array("28", minor)                    => (minor.toInt, false)
      case Array("28", minor, "experimental", _) => (minor.toInt, true)
      case other => sys.error(s"Invalid TASTy version string: $expectedTastyVersion")
    }

    if(isNightly) {
      assert(tastyIsExperimental, "TASTY needs to be experimental in nightly builds")
      val expectedTastyMinor = version.patch match {
        case 0 => version.minor
        case 1 if referenceV.patch == 0 && referenceV.isRC =>
          // Special case for a period when reference version is a new unstable minor
          // Needed for non_bootstrapped tests requiring either stable tasty or the same experimental version produced by both reference and bootstrapped compiler
          assert(version.minor == referenceV.minor, "Expected reference and base version to use the same minor")
          version.minor
        case _ => version.minor + 1
      }
      assert(tastyMinor == expectedTastyMinor, "Invalid TASTy minor version")
    }

    if(isRelease) {
      assert(version.minor == tastyMinor, "Minor versions of TASTY vesion and Scala version should match in release builds")
      assert(!referenceV.isRC, "Stable release needs to use stable compiler version")
      if (version.isRC && version.patch == 0)
        assert(tastyIsExperimental, "TASTy should be experimental when releasing a new minor version RC")
      else
        assert(!tastyIsExperimental, "Stable version cannot use experimental TASTY")
    }
  }

  val scalaJSVersion = "1.20.1"

  /* The Minimum JVM version the artifact should be able to use */
  val minimumJVMVersion = "17"

  val junitJupiter = "5.11.4"

  val inkuireVersion = "v1.0.0-M9"

  val flexmarkVersion = "0.62.2"
  val jacksonVersion = "2.15.1"
}
