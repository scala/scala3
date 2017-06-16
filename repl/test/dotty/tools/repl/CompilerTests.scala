package dotty.tools
package repl

import org.junit.Assert._
import org.junit.Test

import dotc.ast.untpd

import results._

class ReplCompilerTests extends ReplTest {
  @Test def compileSingle = {
    val parsed @ Parsed(_,_) = ParseResult("def foo: 1 = 1")(myCtx)
    val res = compiler.compile(parsed, State.initial(myCtx))
    assert(res.isInstanceOf[State],
      s"Assumed value of `typeCheck` would be TypedTrees - but got: $res")
  }

  @Test def compileTwo = {
    val parsed @ Parsed(_,_) = ParseResult("def foo: 1 = 1")(myCtx)

    compiler
      .compile(parsed, State.initial(myCtx))
      .flatMap { state =>
        val parsed @ Parsed(_,_) = ParseResult("def foo(i: Int): i.type = i")(state.ictx)
        compiler.compile(parsed, state.copy(ictx = myCtx))
      }
      .fold(
        error =>
          fail(s"Expected no errors, got: \n${ error.msgs.map(_.message).mkString("\n") }"),
        state =>
          assert(state.objectIndex == 2,
            s"Wrong object offset: expected 2 got ${state.objectIndex}")
      )
  }

  @Test def inspectSingle = {
    val parsed @ Parsed(_,_) = ParseResult("def foo: 1 = 1")(myCtx)
    val res = for {
      stateAndTrees <- compiler.freeToAssigned(parsed.trees, State.initial(myCtx))
      (State(objectIndex, _, _, ictx), trees) = stateAndTrees
      unit <- compiler.createUnit(trees, objectIndex, parsed.sourceCode)(myCtx)
    } yield (unit.untpdTree, ictx)

    res.fold(
      error => fail(s"received errors: ${error.msgs}"),
      (tree, ictx) => {
        implicit val ctx = ictx

        tree match {
          case untpd.PackageDef(_, List(mod: untpd.ModuleDef)) =>
            assert(mod.name.show == "ReplSession$0", mod.name.show)
          case _ => fail(s"Unexpected structure: $tree")
        }
      }
    )
  }
}
