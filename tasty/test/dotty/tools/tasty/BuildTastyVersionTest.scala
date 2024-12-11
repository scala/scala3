package dotty.tools.tasty

import org.junit.Assert._
import org.junit.Test

import TastyBuffer._

// Tests ensuring TASTY version emitted by compiler is matching expected TASTY version
class BuildTastyVersionTest {
  
  val CurrentTastyVersion = TastyVersion(TastyFormat.MajorVersion, TastyFormat.MinorVersion, TastyFormat.ExperimentalVersion)
  
  // Needs to be defined in build Test/envVars
  val ExpectedTastyVersionEnvVar = "EXPECTED_TASTY_VERSION"
  
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
}
