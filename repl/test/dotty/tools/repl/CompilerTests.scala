package dotty.tools
package repl

import org.junit.Assert._
import org.junit.Test

class ReplCompilerTests extends ReplTest {
  @Test def typeCheck = {
    val parsed @ Parsed(_,_) = ParseResult("def foo: 1 = 1")(myCtx)
    val compiler = new ReplCompiler(myCtx)
    val res = compiler.compile(parsed, State.initial(myCtx))(myCtx)
    assert(res.isInstanceOf[State],
      s"Assumed value of `typeCheck` would be TypedTrees - but got: $res")
  }
}
