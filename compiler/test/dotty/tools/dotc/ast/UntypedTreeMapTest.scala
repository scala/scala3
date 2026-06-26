package dotty.tools
package dotc
package ast

import org.junit.Test
import org.junit.Assert.*

import dotc.core.Contexts.*
import dotc.parsing.Parsers.Parser
import dotc.util.SourceFile

class UntpdTreeMapTest extends DottyTest {

  import untpd.*

  def parse(code: String): Tree = {
    val (_, stats) = new Parser(SourceFile.virtual("<meta>", code)).templateStatSeq()
    stats match { case Vector(stat) => stat; case stats => untpd.Thicket(stats) }
  }

  def parseStat(code: String): Tree = parse(code)

  def parseStats(count: Int): Vector[Tree] =
    (0 until count).map(i => parseStat(s"val x$i = $i")).toVector

  @Test
  def testMapInterpolatedString = {
    val tree = parse(""" q"hello ${2017}!" """)
    val identity = new UntypedTreeMap {
      override def transform(tree: Tree)(using Context): Tree = tree match {
        case _ =>  super.transform(tree)
      }
    }

    assertEquals(tree.toString, identity.transform(tree).toString)
  }

  @Test
  def flattenedMapConservePreservesIdentity: Unit =
    val stats = parseStats(33)
    val mapped = Trees.flattenedMapConserve(stats)(identity)
    assertSame(stats, mapped)

  @Test
  def flattenedMapConserveReplacesOneTreeAtVector1Boundary: Unit =
    val stats = parseStats(32)
    val replacement = parseStat("val y = 99")
    val mapped = Trees.flattenedMapConserve(stats)(tree =>
      if tree eq stats(31) then replacement else tree
    )
    assertNotSame(stats, mapped)
    assertEquals(32, mapped.length)
    assertSame(stats(30), mapped(30))
    assertSame(replacement, mapped(31))

  @Test
  def flattenedMapConserveReplacesOneTreeAboveVector1Boundary: Unit =
    val stats = parseStats(33)
    val replacement = parseStat("val y = 99")
    val mapped = Trees.flattenedMapConserve(stats)(tree =>
      if tree eq stats(32) then replacement else tree
    )
    assertNotSame(stats, mapped)
    assertEquals(33, mapped.length)
    assertSame(stats(31), mapped(31))
    assertSame(replacement, mapped(32))

  @Test
  def flattenedMapConserveFlattensThicket: Unit =
    val stats = parseStats(4)
    val first = parseStat("val y1 = 1")
    val second = parseStat("val y2 = 2")
    val mapped = Trees.flattenedMapConserve(stats)(tree =>
      if tree eq stats(1) then Thicket(Vector(first, second)) else tree
    )
    assertEquals(
      Vector(stats(0), first, second, stats(2), stats(3)).map(_.toString),
      mapped.map(_.toString)
    )
    assertSame(stats(0), mapped(0))
    assertSame(first, mapped(1))
    assertSame(second, mapped(2))

  @Test
  def flattenedMapConserveFlattensThicketAfterReplacement: Unit =
    val stats = parseStats(5)
    val replacement = parseStat("val y = 99")
    val first = parseStat("val z1 = 1")
    val second = parseStat("val z2 = 2")
    val mapped = Trees.flattenedMapConserve(stats)(tree =>
      if tree eq stats(1) then replacement
      else if tree eq stats(3) then Thicket(Vector(first, second))
      else tree
    )
    assertEquals(
      Vector(stats(0), replacement, stats(2), first, second, stats(4)).map(_.toString),
      mapped.map(_.toString)
    )
    assertSame(replacement, mapped(1))
    assertSame(first, mapped(3))
    assertSame(second, mapped(4))
}
