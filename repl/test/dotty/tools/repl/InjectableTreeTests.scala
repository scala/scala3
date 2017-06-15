package dotty.tools
package repl

import org.junit.Assert._
import org.junit.Test
import dotc.ast.tpd

class InjectableTreeTests extends ReplTest {
  @Test def crashCheck = {
    implicit val ctx = myCtx
    val injTree = InjectableTree()
    val parsed @ Parsed(_,_) = ParseResult("def foo: 1 = 1")
    val symName = injTree.obj.trees(1).symbol.show
    assert(symName == "object ReplSession", symName)
  }

  @Test def injectOnce = {
    implicit val ctx = myCtx

    val injTree = InjectableTree()
    val parsed @ Parsed(_,_) = ParseResult("def foo: 1 = 1")
    val typer = new ReplTyper(myCtx)

    val tt @ TypedTrees(_,_) = typer.typeCheck(parsed, 0)
    val newInjTree @ InjectableTree(_,_) = InjectableTree.patch(injTree, tt)

    // assert( newInjTree has 1 method == "def foo: 1 = 1" )
  }
}
