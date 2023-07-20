package dotty.tools.pc.tests.hover

import dotty.tools.pc.base.BaseHoverSuite

import org.junit.Test

class HoverNegativeSuite extends BaseHoverSuite:

  // Negative results should have an empty output.
  def checkNegative(original: String): Unit = check(original, expected = "")

  @Test def `block` =
    checkNegative(
      """object a {
        |  val x = {
        |    @@
        |    List(y)
        |  }
        |}
        |""".stripMargin
    )

  @Test def `template` =
    checkNegative(
      """object a {
        |    @@
        |  def foo = 2
        |}
        |""".stripMargin
    )

  @Test def `block2` =
    checkNegative(
      """object a {
        |  def foo = {
        |    val x = 2
        |    @@
        |    x
        |  }
        |}
        |""".stripMargin
    )

  @Test def `val-keyword` =
    checkNegative(
      """object a {
        |  v@@al x = 42
        |}
        |""".stripMargin
    )

  @Test def `val-equal` =
    checkNegative(
      """object a {
        |  val x =@@ 42
        |}
        |""".stripMargin
    )

  @Test def `literal-int` =
    checkNegative(
      """object a {
        |  val x = 4@@2
        |}
        |""".stripMargin
    )

  @Test def `literal-double` =
    checkNegative(
      """object a {
        |  val x = 4@@2d
        |}
        |""".stripMargin
    )

  @Test def `literal-float` =
    checkNegative(
      """object a {
        |  val x = 4@@2f
        |}
        |""".stripMargin
    )

  @Test def `literal-long` =
    checkNegative(
      """object a {
        |  val x = 4@@2L
        |}
        |""".stripMargin
    )

  @Test def `literal-string` =
    checkNegative(
      """object a {
        |  val x = "Hel@@lo"
        |}
        |""".stripMargin
    )

  @Test def `interpolator-part` =
    checkNegative(
      """object a {
        |  val name = "John"
        |  s"Hel@@lo $name"
        |}
        |""".stripMargin
    )
