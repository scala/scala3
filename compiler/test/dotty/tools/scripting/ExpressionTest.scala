package dotty
package tools
package scripting

import scala.language.unsafeNulls

import java.nio.file.Paths
import org.junit.{Test, AfterClass}
import org.junit.Assert.assertEquals
import org.junit.Assume.assumeFalse
import org.junit.experimental.categories.Category

import vulpix.TestConfiguration

import ScriptTestEnv.*

/**
 *   +. test scala -e <expression>
 */
@Category(Array(classOf[BootstrappedOnlyTests]))
class ExpressionTest:
  /*
   * verify -e <expression> works.
   */
  @Test def verifyCommandLineExpression =
    assumeFalse("Scripts do not yet support Scala 2 library TASTy", Properties.usingScalaLibraryTasty)
    printf("===> verify -e <expression> is properly handled by `dist/bin/scala`\n")
    val expected = "9"
    val expression = s"println(3*3)"
    val result = getResult(expression)
    assert(result.contains(expected), s"expression [$expression] did not send [$expected] to stdout. It send [$result].")

  @Test def verifyImports: Unit =
    assumeFalse("Scripts do not yet support Scala 2 library TASTy", Properties.usingScalaLibraryTasty)
    val expressionLines = List(
      "import java.nio.file.Paths",
      "import scala.util.Properties.userDir",
      "println(Paths.get(userDir).toFile.listFiles.toList.filter(_.isDirectory).size)",
    )
    val expression = expressionLines.mkString(";")
    val success = testExpression(expression){ result =>
      result.matches("[0-9]+") && result.toInt > 0
    }
    assert(success)

  def getResult(expression: String): String =
    val (_, _, stdout, stderr) = bashCommand(s"$scalaPath -e '$expression' --power --offline --server=false")
    printf("stdout: %s\n", stdout.mkString("|"))
    printf("stderr: %s\n", stderr.mkString("\n", "\n", ""))
    stdout.filter(_.nonEmpty).mkString("")

  def testExpression(expression: String)(check: (result: String) => Boolean): Boolean =
    val result = getResult(expression)
    check(result)

object ExpressionTest:

  @AfterClass def cleanup(): Unit = {
    cleanupScalaCLIDirs()
  }

  def main(args: Array[String]): Unit =
    val tests = new ExpressionTest
    println("\n=== verifyCommandLineExpression ===")
    tests.verifyCommandLineExpression
    println("\n=== verifyImports ===")
    tests.verifyImports
