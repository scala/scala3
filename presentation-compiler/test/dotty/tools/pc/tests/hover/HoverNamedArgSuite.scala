package dotty.tools.pc.tests.hover

import org.junit.{Ignore, Test}
import dotty.tools.pc.base.BaseHoverSuite

class HoverNamedArgSuite extends BaseHoverSuite:
  override protected def requiresScalaLibrarySources: Boolean = true
  override protected def requiresJdkSources: Boolean = true

  @Ignore // TODO SemanticdbSymbols.inverseSemanticdbSymbol does not support params and type params search
  @Test def `named` =
    check(
      """package a
        |object b {
        |  /**
        |   * Runs foo
        |   * @param named the argument
        |   */
        |  def foo(named: Int): Unit = ()
        |  <<foo(nam@@ed = 2)>>
        |}
        |""".stripMargin,
      """|```scala
         |named: Int
         |```
         |Runs foo
         |
         |**Parameters**
         |- `named`: the argument
         |""".stripMargin
    )

  @Test def `error` =
    check(
      """package a
        |object c {
        |  def foo(a: String, named: Int): Unit = ()
        |  foo(nam@@ed = 2)
        |}
        |""".stripMargin,
      ""
    )

  @Test def `error2` =
    check(
      """package a
        |object d {
        |  def foo(a: Int, named: Int): Unit = ()
        |  foo("error", nam@@ed = 2)
        |}
        |""".stripMargin,
      "named: Int".hover
    )

  @Test def `nested` =
    check(
      """package a
        |object e {
        |  class User(name: String, age: Int)
        |  println(<<new User(age = 42, n@@ame = "")>>)
        |}
        |""".stripMargin,
      "name: String".hover
    )
