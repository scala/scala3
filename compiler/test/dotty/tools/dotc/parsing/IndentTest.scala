package dotty.tools
package dotc
package parsing

import org.junit.Test

class IndentTest extends ParserTest:

  @Test
  def parseBraces: Unit =
    val code = s"""
      |class A {
      |  val x = 1
      |    val y = 2
      |}""".stripMargin
    assert(parseTextEither(code).isRight)

  @Test
  def parseIndents: Unit =
    val code = s"""
      |class A:
      |  val x = 1
      |  val y = 2
      |""".stripMargin
    assert(parseTextEither(code).isRight)

  @Test
  def innerClassIndents: Unit =
    val code = s"""
      |class A:
      |  class B:
      |    val x = 1
      |""".stripMargin
    assert(parseTextEither(code).isRight)

  @Test
  def extendsClassIndents: Unit =
    val code = s"""
      |class A extends B:
      |  override def hasUnreportedErrors: Boolean =
      |    infos.exists(_.isInstanceOf[Error])
      |""".stripMargin
    assert(parseTextEither(code).isRight)

  @Test
  def superfluousIndents: Unit =
    val code = s"""
      |class A:
      |  val x = 1
      |    val y = 2
      |""".stripMargin
    assert(parseTextEither(code).isLeft)

  @Test
  def superfluousIndents2: Unit =
    val code = s"""
      |class Test:
      |  test("hello")
      |    assert(1 == 1)
      |""".stripMargin
    assert(parseTextEither(code).isLeft)
