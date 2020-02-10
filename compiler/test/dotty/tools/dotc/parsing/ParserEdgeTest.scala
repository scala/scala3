package dotty.tools
package dotc
package parsing

import ast.untpd._
import core.Constants._

import org.junit.Test

class ParserEdgeTest extends ParserTest {
  //
  // CR after right brace of interpolated expression was stripped.
  //
  @Test def `t9944 respect line separator`: Unit = {
    // avoid git stripping CR from a test file; also, inlining doesn't demonstrate the behavior.
    val CR = "\u000D"
    val NL = "\u000A"
    val triple = "\"" * 3
    val text = s"""class C { def g = "X" ; def f = s${triple}123${CR}${NL}$${g}${CR}${NL}456${triple} }"""
    def isStripped(s: String) = s.contains(NL) && !s.contains(CR)

    // sanity check
    assert(text.linesIterator.size == 3, s"line count ${text.linesIterator.size}")
    assert(text.linesIterator.forall(!isStripped(_)))

    val t = parseText(text)
    //println(t.show)
    assert(!t.existsSubTree {
      case Literal(const @ Constant(_)) if const.tag == StringTag => isStripped(const.stringValue)
      case st => false
    })
  }
  /* was:
  package <empty> {
    class C {
      def g = "X"
      def f =
        s"123\r\n{
          {
            g
          }
        }\n456"
    }
  }
   * now:
  package <empty> {
    class C {
      def g = "X"
      def f =
        s"123\r\n{
          {
            g
          }
        }\r\n456"
    }
  }
   * tests/run/t9944 expressed ordinarily, with CR as indicated:
class C {
  def g = 42
  def f =
    s"""123^M
       |${ g }^M
       |123""".stripMargin
}

object Test extends App {
  println(new C().f.getBytes.toList.map(c => f"$c%02x"))
}
   * was:
➜ dotr Test
List(31, 32, 33, 0d, 0a, 34, 32, 0a, 31, 32, 33)
   */
}
