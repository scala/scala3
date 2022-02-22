package dotty.tools.dotc.typer

import dotty.tools.DottyTest
import dotty.tools.dotc.core.Contexts._

import org.junit.Test
import org.junit.Assert.{ assertTrue, fail }

class DivergenceCheckerTests extends DottyTest {
  @Test
  def testCoveringSet: Unit = {
    val source = """
      |class A
      |class B extends A
    """.stripMargin

    val types = List(
      "A",
      "B",
      "List[?]",
      "List[Int]",
      "List[AnyRef]",
      "List[String]",
      "List[List[(A, B)]]",
      "List[List[(A, B) { type Baz = Int }]] { type Foo = A }"
    )

    val elements = List("A", "B", "Nothing", "Object", "String", "Tuple2[?, ?]", "Int", "List[?]")

    checkTypes(source, List(types, elements)) {
      case (List(tpes, elements0), context) =>
        given Context = context

        val List(a, b, n, o, s, t2, i, l) = elements0.map(_.typeConstructor)
        val expectedCoveringSets = List(
          Set(a),
          Set(b),
          Set(l),
          Set(l, i),
          Set(l, o),
          Set(l, s),
          Set(l, t2, a, b),
          Set(l, t2, a, b, i)
        ).map(_.map(_.dealias.typeSymbol))

        val expectedSizes = List(
          0,
          0,
          1,
          1,
          1,
          1,
          3,
          5
        )

        tpes.lazyZip(expectedSizes).lazyZip(expectedCoveringSets).foreach {
          case (tpe, expectedSize, expectedCoveringSet) =>
            val size = tpe.typeSize
            val cs = tpe.coveringSet

            assertTrue(size == expectedSize)
            assertTrue(cs == expectedCoveringSet)
        }

      case _ => fail
    }
  }
}
