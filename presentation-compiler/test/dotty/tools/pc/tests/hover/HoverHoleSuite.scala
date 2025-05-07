package dotty.tools.pc.tests.hover

import dotty.tools.pc.base.BaseHoverSuite

import org.junit.Test

class HoverHoleSuite extends BaseHoverSuite:
  @Test def basic =
    check(
      """object a {
        |  val x: Int = ?@@??
        |}
        |""".stripMargin,
      """|**Expression type**:
         |```scala
         |Int
         |```
         |**Symbol signature**:
         |```scala
         |def ???: Nothing
         |```
         |""".stripMargin
    )

  @Test def function =
    check(
      """object a {
        |  def m(i: Int) = ???
        |  val x = m(??@@?)
        |}
        |""".stripMargin,
      """|**Expression type**:
         |```scala
         |Int
         |```
         |**Symbol signature**:
         |```scala
         |def ???: Nothing
         |```
         |""".stripMargin
    )

  @Test def constructor =
    check(
      """object a {
        |  val x = List(1, ?@@??)
        |}
        |""".stripMargin,
      """|**Expression type**:
         |```scala
         |Int
         |```
         |**Symbol signature**:
         |```scala
         |def ???: Nothing
         |```
         |""".stripMargin
    )

  @Test def bounds =
    check(
      """|trait Foo
         |def foo[T <: Foo](a: T): Boolean = ???
         |val _ = foo(?@@??)
         |""".stripMargin,
      """|**Expression type**:
         |```scala
         |Foo
         |```
         |**Symbol signature**:
         |```scala
         |def ???: Nothing
         |```
         |""".stripMargin
    )
