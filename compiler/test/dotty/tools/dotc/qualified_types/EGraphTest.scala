package dotty.tools.dotc.qualified_types

import dotty.tools.DottyTest
import dotty.tools.dotc.core.Decorators.i
import dotty.tools.dotc.ast.tpd

import org.junit.Assert.assertEquals
import org.junit.Test

class EGraphTest extends QualifiedTypesTest:

  def checkImplies(fromString: String, toString: String, egraphString: String, expected: Boolean = true): Unit =
    val src = s"""
      |def test = {
      |  val b1: Boolean = ???
      |  val b2: Boolean = ???
      |  val b3: Boolean = ???
      |  val w: Int = ???
      |  val x: Int = ???
      |  val y: Int = ???
      |  val z: Int = ???
      |  def f(a: Int): Boolean = ???
      |  def g(a: Int): Int = ???
      |  def h(a: Int, b: Int): Int = ???
      |  def id[T](a: T): T = a
      |  type Vec[T]
      |  type Pos = {v: Int with v > 0}
      |  def len[T](v: Vec[T]): Pos = ???
      |  val v1: Vec[Int] = ???
      |  val v2: Vec[Int] = ???
      |  val v3: Vec[Int] = ???
      |  val from: Boolean = $fromString
      |  val to: Boolean = $toString
      |}""".stripMargin
    checkCompileExpr(src): stats =>
      val testTree = getDefDef(stats, "test")
      val body = testTree.rhs.asInstanceOf[tpd.Block]
      val fromTree = getValDef(body.stats, "from").rhs
      val toTree = getValDef(body.stats, "to").rhs
      val egraph = EGraph(ctx, checksEnabled = true)
      val from = ENode.fromTree(fromTree).get.normalizeTypes()
      val to = ENode.fromTree(toTree).get.normalizeTypes()
      val fromCanonical = egraph.canonicalize(from)
      val toCanonical = egraph.canonicalize(to)
      egraph.merge(fromCanonical, egraph.trueNode)
      egraph.repair()
      assertStringEquals(egraphString, egraph.debugString())
      val res = egraph.equiv(toCanonical, egraph.trueNode)
      assertEquals(s"Expected $fromString  -->  $toString to be $expected", expected, res)

  def checkNotImplies(fromString: String, toString: String, egraphString: String): Unit =
    checkImplies(fromString, toString, egraphString, expected = false)

  @Test def test1() =
    checkImplies(
      "true",
      "true",
      """-1: {}
        |0: {}
        |1: {}
        |false: {}
        |true: {}
        |""".stripMargin
    )

  @Test def test2() =
    checkImplies(
      "b1",
      "b1",
      """-1: {}
        |0: {}
        |1: {}
        |false: {}
        |true: {b1}
        |""".stripMargin
    )

  @Test def test3() =
    checkNotImplies(
      "b1",
      "b2",
      """-1: {}
        |0: {}
        |1: {}
        |b2: {}
        |false: {}
        |true: {b1}
        |""".stripMargin
    )

  @Test def test4() =
    checkImplies(
      "b1 && b2",
      "b2",
      """-1: {}
        |0: {}
        |1: {}
        |false: {}
        |true: {b1, b1 && b2, b2}
        |""".stripMargin
    )

  @Test def test5() =
    checkNotImplies(
      "b1 || b2",
      "b2",
      """-1: {}
        |0: {}
        |1: {}
        |b1: {}
        |b2: {}
        |false: {}
        |true: {b1 || b2}
        |""".stripMargin
    )

  @Test def test6() =
    checkImplies(
      "b1 && b2 && b3",
      "b3",
      """-1: {}
        |0: {}
        |1: {}
        |false: {}
        |true: {b1, b1 && b2, b1 && b2 && b3, b2, b3}
        |""".stripMargin
    )

  @Test def test7() =
    checkImplies(
      "b1 && b1 == b2",
      "b2",
      """-1: {}
        |0: {}
        |1: {}
        |false: {}
        |true: {b1, b1 && b1 == b2, b1 == b2, b2}
        |""".stripMargin
    )

  @Test def test8() =
    checkImplies(
      "b1 && b1 == b2 && b2 == b3",
      "b3",
      """-1: {}
        |0: {}
        |1: {}
        |false: {}
        |true: {b1, b1 && b1 == b2, b1 && b1 == b2 && b2 == b3, b1 == b2, b2, b2 == b3, b3}
        |""".stripMargin
    )

  @Test def test9() =
    checkImplies(
      "f(x) && x == y",
      "f(y)",
      """-1: {}
        |0: {}
        |1: {}
        |f: {}
        |false: {}
        |true: {f(x), f(x) && x == y, f(y), x == y}
        |x: {y}
        |""".stripMargin
    )

  @Test def nestedFunctions() =
    checkImplies(
      "f(g(x)) && g(x) == g(y)",
      "f(g(y))",
      """-1: {}
        |0: {}
        |1: {}
        |f: {}
        |false: {}
        |g: {}
        |g(x): {g(y)}
        |true: {f(g(x)), f(g(x)) && g(x) == g(y), f(g(y)), g(x) == g(y)}
        |x: {}
        |y: {}
        |""".stripMargin
    )

  @Test def multipleArgs() =
    checkImplies(
      "y == z",
      "h(x, y) == h(x, z)",
      """-1: {}
        |0: {}
        |1: {}
        |false: {}
        |h: {}
        |h(x, y): {h(x, z)}
        |true: {h(x, y) == h(x, z), y == z}
        |x: {}
        |y: {z}
        |""".stripMargin
    )

  @Test def multipleArgsDeep() =
    checkImplies(
      "f(h(x, y)) && y == z",
      "f(h(x, z))",
      """-1: {}
        |0: {}
        |1: {}
        |f: {}
        |false: {}
        |h: {}
        |h(x, y): {h(x, z)}
        |true: {f(h(x, y)), f(h(x, y)) && y == z, f(h(x, z)), y == z}
        |x: {}
        |y: {z}
        |""".stripMargin
    )

  @Test def sizeSum() =
    checkImplies(
      "len(v1) == len(v2) + len(v3) && len(v2) == 3 && len(v3) == 4",
      "len(v1) == 7",
      """-1: {}
        |0: {}
        |1: {}
        |3: {len[Int](v2)}
        |4: {len[Int](v3)}
        |7: {len[Int](v1), len[Int](v2) + len[Int](v3)}
        |false: {}
        |len: {}
        |len[Int]: {}
        |true: {len[Int](v1) == 7, len[Int](v1) == len[Int](v2) + len[Int](v3), len[Int](v1) == len[Int](v2) + len[Int](v3) && len[Int](v2) == 3, len[Int](v1) == len[Int](v2) + len[Int](v3) && len[Int](v2) == 3 && len[Int](v3) == 4, len[Int](v2) + len[Int](v3) == 7, len[Int](v2) == 3, len[Int](v3) == 4}
        |v1: {}
        |v2: {}
        |v3: {}
        |""".stripMargin
    )
