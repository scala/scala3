package dotty.tools
package dotc
package parsing

import ast.untpd._
import org.junit.Test

class IndentTest extends ScannerTest:

  @Test
  def parseBraces: Unit =
    val code = s"""
      |class A {
      |  val x = 1
      |    val y = 2
      |}""".stripMargin
    assert(scanTextEither(code).isRight)

  @Test
  def parseIndents: Unit =
    val code = s"""
      |class A:
      |  val x = 1
      |""".stripMargin
    assert(scanTextEither(code).isRight)

  @Test
  def superfluousIndents: Unit =
    val code = s"""
      |class A:
      |  val x = 1
      |    val y = 2
      |""".stripMargin
    assert(scanTextEither(code).isLeft)
