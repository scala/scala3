package dotty.tools
package repl

import org.junit.Assert._
import org.junit.Test

import dotc.reporting.MessageRendering
import dotc.ast.untpd

import results._
import ReplTest._

class ReplCompilerTests extends ReplTest with MessageRendering {


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

  @Test def testVar = {
    val parsed @ Parsed(_,_) = ParseResult("var x = 5")(myCtx)
    compile(parsed, initState)
    assertEquals("var x: Int = 5\n", stripColor(storedOutput()))
  }

  @Test def testRes = {
    implicit val ctx = myCtx
    val parsed @ Parsed(_,_) = ParseResult(
      """|def foo = 1 + 1
         |val x = 5 + 5
         |1 + 1
         |var y = 5
         |10 + 10
         |class Foo""".stripMargin
    )

    compile(parsed, initState)


    val expected = Set("def foo: Int",
                       "val x: Int = 10",
                       "val res1: Int = 20",
                       "val res0: Int = 2",
                       "var y: Int = 5",
                       "// defined class Foo")

    expected === stripColor(storedOutput()).split("\n")
  }
}
