package dotty.tools
package repl

import org.junit.Test
import org.junit.Assert._

class TypeTests extends ReplTest {
  @Test def typeOf1 =
    compiler
      .typeOf("1", initState, rootCtx)
      .fold(onErrors, tpe => assertEquals("Int", tpe))

  @Test def typeOfBlock =
    compiler
      .typeOf("{ /** omg omg omg */ 1 + 5; 1 }", initState, rootCtx)
      .fold(onErrors, tpe => assertEquals("Int", tpe))

  @Test def typeOfX = {
    val parsed = ParseResult("val x = 5")(rootCtx).asInstanceOf[Parsed]
    val state = compile(parsed, initState)

    compiler
      .typeOf("x", state, state.ctx)
      .fold(onErrors, tpe => assertEquals("Int", tpe))
  }
}
