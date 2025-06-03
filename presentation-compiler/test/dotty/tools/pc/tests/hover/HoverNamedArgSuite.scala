package dotty.tools.pc.tests.hover

import scala.meta.pc.SymbolDocumentation

import dotty.tools.pc.base.BaseHoverSuite
import dotty.tools.pc.utils.MockEntries

import org.junit.Test

class HoverNamedArgSuite extends BaseHoverSuite:

  override protected def mockEntries: MockEntries = new MockEntries:
    override def documentations: Set[SymbolDocumentation] = Set(
      ScalaMockDocumentation("a/b.foo().(named)", "foo", List(), List(MockParam("named")))
    )

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
