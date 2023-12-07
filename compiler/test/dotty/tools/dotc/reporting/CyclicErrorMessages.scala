package dotty.tools
package dotc
package reporting

import dotty.tools.dotc.core.Contexts.Context
import org.junit.Assert._
import org.junit.Test

/*better error message for cyclic errors when using exports as in #17076 */
class CyclicErrorMessages extends ErrorMessagesTest {
  @Test def cyclicErrMsg =
    checkMessagesAfter("typer") {
            """object A {
              |  def bar(x: B.Foo[Int]) = x
              |}
              |
              |object B {
              |  import C.*
              |  case class Foo[T <: Int](x: Any)
              |  def foo = Foo(0)
              |}
              |
              |object C extends D.Foo[Int](0)
              |
              |object D {
              |  export B.foo
              |  type Foo[T <: Int] = B.Foo[T]
              |}
      """.stripMargin
    }.expect { (itcx, messages) =>
      given Context = itcx

      assertMessageCount(2, messages)
      val msg = messages.head.message

      assertTrue(msg.contains("Compiler bug: unknown cyclic error"))
    }
}
