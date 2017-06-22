package dotty.tools
package repl

import org.junit.Assert._
import org.junit.Test

import dotc.ast.untpd

import results._

class ReplCompilerTests extends ReplTest {

  @Test def compileSingle = {
    val parsed @ Parsed(_,_) = ParseResult("def foo: 1 = 1")(myCtx)
    compiler
      .compile(parsed, State(0, 0, Nil))
      .fold(onErrors(_), _ => ())
  }

  @Test def compileTwo = {
    implicit val ctx = myCtx
    val parsed @ Parsed(_,_) = ParseResult("def foo: 1 = 1")(myCtx)

    compiler
      .compile(parsed, State(0, 0, Nil))
      .flatMap { (unit, state, ctx) =>
        val parsed @ Parsed(_,_) = ParseResult("def foo(i: Int): i.type = i")(ctx)
        compiler.compile(parsed, state)
      }
      .fold(
        onErrors(_),
        (unit, state, ctx) => {
          assert(state.objectIndex == 2,
            s"Wrong object offset: expected 2 got ${state.objectIndex}")
        }
      )
  }

  @Test def inspectSingle = {
    implicit val ctx = myCtx
    val parsed @ Parsed(_,_) = ParseResult("def foo: 1 = 1")
    val res = for {
      defs <- compiler.definitions(parsed.trees, State(0, 0, Nil))
      unit <- compiler.createUnit(defs.trees, defs.state.objectIndex, parsed.sourceCode)
    } yield unit.untpdTree

    res.fold(
      onErrors(_),
      tree => {
        implicit val ctx = myCtx

        tree match {
          case untpd.PackageDef(_, List(mod: untpd.ModuleDef)) =>
            assert(mod.name.show == "ReplSession$0", mod.name.show)
          case _ => fail(s"Unexpected structure: $tree")
        }
      }
    )
  }
}
