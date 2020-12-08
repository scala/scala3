package dotty.tools
package dotc
package parsing

import org.junit.Test

class IndentWidthTest extends ScannerTest:

  @Test
  def innerObjectIndents: Unit =
    val code = s"""
      |object A:
      |  object B
      |  end B
      |
      |  object C
      |""".stripMargin
    assert(scanTextEither(code).isRight)
