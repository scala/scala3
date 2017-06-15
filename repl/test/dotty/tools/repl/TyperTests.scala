package dotty.tools
package repl

import org.junit.Assert._
import org.junit.Test

class ReplTyperTests extends ReplTest {
  @Test def typeCheck = {
    val parsed @ Parsed(_,_) = ParseResult("def foo: 1 = 1")(myCtx)
    val typer = new ReplTyper(myCtx)
    val res = typer.typeCheck(parsed, 0)(myCtx)
    assert(res.isInstanceOf[TypedTrees],
      s"Assumed value of `typeCheck` would be TypedTrees - but got: $res")
  }
}
