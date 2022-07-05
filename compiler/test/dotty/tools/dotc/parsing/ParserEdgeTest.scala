package dotty.tools
package dotc
package parsing

import ast.untpd.*
import core.Constants.*
import core.Decorators.*

import org.junit.Test
import org.junit.Assert.assertTrue

class ParserEdgeTest extends ParserTest {
  def verifyXML(text: String)(p: Tree => Boolean): Unit =
    val t = parseText(text)
    t.checkPos(nonOverlapping = true)
    assertTrue("predicate failed", p(t))

  /*@Test*/ def `i6547 xml blocks should be correctly positioned`: Unit =
    val code =
      i"""
        |class Foo {
        |  <foo a="hello &name; aaa"/>
        |}
      """
    verifyXML(code)(_ => true) // per comments in SymbolicXMLBuilder and Positioned, requires reworking

  @Test def `i15574 span of xml literal with windows line separators`: Unit =
    val code =
      i"""
        |object Test {
        |  Nil.map { _ =>
        |    <X />
        |    <Y />
        |  }
        |}
      """
    extension (text: String) def convertToLineSeparator(sep: String): String = text.linesIterator.map(_ + sep).mkString
    val crlfCode = code.convertToLineSeparator("\r\n")
    val lfCode = code.convertToLineSeparator("\n")
    def verifyTreeHasSpans(text: String): Unit =
      val x0 = text.indexOf('<')
      val x1 = text.indexOf('>', x0+1) + 1
      val y0 = text.indexOf('<', x1+1)
      val y1 = text.indexOf('>', y0+1) + 1
      def treeHasSpan(tree: Tree, start: Int, end: Int): Boolean =
        tree.existsSubTree(t => t.span.exists && t.span.start == start && t.span.end == end)
      verifyXML(text)(tree => treeHasSpan(tree, x0, x1) && treeHasSpan(tree, y0, y1))

    assert(crlfCode != lfCode) // check construction
    verifyTreeHasSpans(crlfCode)
    verifyTreeHasSpans(lfCode)

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

    // check construction
    assert(text.linesIterator.size == 3, s"line count ${text.linesIterator.size}")
    assert(text.linesIterator.forall(!isStripped(_)))

    val t = parseText(text)
    //println(t.show)
    assertTrue(!t.existsSubTree {
      case Literal(const @ Constant(_)) if const.tag == StringTag => isStripped(const.stringValue)
      case _ => false
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
