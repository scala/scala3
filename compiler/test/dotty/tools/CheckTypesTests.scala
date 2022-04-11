package dotty.tools

import org.junit.Test
import org.junit.Assert.{ assertFalse, assertTrue, fail }

import dotc.core.Contexts.Context

class CheckTypeTest extends DottyTest {
  @Test
  def checkTypesTest: Unit = {
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
      "List[A]",
      "List[B]"
    )

    checkTypes(source, types: _*) {
      case (List(a, b, lu, li, lr, ls, la, lb), context) =>
        given Context = context

        assertTrue  ( b <:<  a)
        assertTrue  (li <:< lu)
        assertFalse (li <:< lr)
        assertTrue  (ls <:< lr)
        assertTrue  (lb <:< la)
        assertFalse (la <:< lb)

      case _ => fail
    }
  }

  @Test
  def checkTypessTest: Unit = {
    val source = """
      |class A
      |class B extends A
    """.stripMargin

    val typesA = List(
      "A",
      "List[A]"
    )

    val typesB = List(
      "B",
      "List[B]"
    )

    checkTypes(source, List(typesA, typesB)) {
      case (List(sups, subs), context) =>
        given Context = context

        sups.lazyZip(subs).foreach { (sup, sub) => assertTrue(sub <:< sup) }

      case _ => fail
    }
  }
}
