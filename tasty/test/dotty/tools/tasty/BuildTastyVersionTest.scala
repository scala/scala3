package dotty.tools.tasty

import org.junit.Assert._
import org.junit.Test

import TastyBuffer._

// Tests ensuring TASTY version emitted by compiler is matching expected TASTY version
class BuildTastyVersionTest {
  
  val CurrentTastyVersion = TastyVersion(TastyFormat.MajorVersion, TastyFormat.MinorVersion, TastyFormat.ExperimentalVersion)
  
  // Needs to be defined in build Test/envVars
  val ExpectedTastyVersionEnvVar = "EXPECTED_TASTY_VERSION"
  val BaseVersionEnvVar = "BASE_VERSION"
  
  @Test def testBuildTastyVersion(): Unit = {
    val expectedVersion = sys.env.get(ExpectedTastyVersionEnvVar)
      .getOrElse(fail(s"Env variable $ExpectedTastyVersionEnvVar not defined"))
      .match {
        case s"$major.$minor-experimental-$experimental" => TastyVersion(major.toInt, minor.toInt, experimental.toInt)
        case s"$major.$minor" if minor.forall(_.isDigit) => TastyVersion(major.toInt, minor.toInt, 0)
        case other => fail(s"Invalid TASTY version string: $other")
      }
    assertEquals(CurrentTastyVersion, expectedVersion)
  }
  
  // Tested only in nightly / release builds
  // Protects from publishing artifacts with incorrect TASTY version
  @Test def testReleasedTastyVersion(): Unit = {
    lazy val (minor, patch, isRC) = sys.env.get(BaseVersionEnvVar)
      .getOrElse(fail(s"Env variable $BaseVersionEnvVar not defined"))
      .match {
        case s"3.$minor.$patch-${_}" => (minor.toInt, patch.toInt, true)
        case s"3.$minor.$patch"      => (minor.toInt, patch.toInt, false)
        case other => fail(s"Invalid Scala base version string: $other")
      }
  
    if sys.env.get("NIGHTLYBUILD").contains("yes") then {
      assertTrue(
        "TASTY needs to be experimental in nightly builds",
        CurrentTastyVersion.isExperimental
      )
      assertEquals(
        CurrentTastyVersion.minor,
        if patch == 0 then minor else (minor + 1)
      )
    } else if sys.env.get("RELEASEBUILD").contains("yes") then {
      assertEquals(
        "Minor versions of TASTY vesion and Scala version should match in stable release",
        CurrentTastyVersion.minor, minor
      )
      if isRC then
        assertEquals(
          "TASTy should be experimental when releasing a new minor version RC",
          CurrentTastyVersion.isExperimental, patch == 0
        )  
      else 
        assertFalse(
          "Stable version cannot use experimental TASTY", 
          CurrentTastyVersion.isExperimental
        )
    }
  }
}
