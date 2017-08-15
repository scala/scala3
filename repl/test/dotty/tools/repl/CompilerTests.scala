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
    val parsed @ Parsed(_,_) = ParseResult("def foo: 1 = 1")(rootCtx)
    compiler
      .compile(parsed, initState, rootCtx)
      .fold(onErrors(_), _ => ())
  }

  @Test def compileTwo = {
    implicit val ctx = rootCtx
    val parsed @ Parsed(_,_) = ParseResult("def foo: 1 = 1")(rootCtx)

    compiler
      .compile(parsed, initState, rootCtx)
      .flatMap { (unit, state) =>
        val parsed @ Parsed(_,_) = ParseResult("def foo(i: Int): i.type = i")(rootCtx)
        compiler.compile(parsed, state, rootCtx)
      }
      .fold(
        onErrors(_),
        (unit, state) => {
          assert(state.objectIndex == 2,
            s"Wrong object offset: expected 2 got ${state.objectIndex}")
        }
      )
  }

  @Test def inspectSingle = {
    implicit val ctx = rootCtx
    val parsed @ Parsed(_,_) = ParseResult("def foo: 1 = 1")
    val res = for {
      (unit, _) <- compiler.compile(parsed, initState, rootCtx)
    } yield unit.untpdTree

    res.fold(
      onErrors(_),
      tree => {
        implicit val ctx = rootCtx

        tree match {
          case untpd.PackageDef(_, List(mod: untpd.ModuleDef)) =>
            assert(mod.name.show == "ReplSession$1", mod.name.show)
          case _ => fail(s"Unexpected structure: $tree")
        }
      }
    )
  }

  @Test def testVar = {
    val parsed @ Parsed(_,_) = ParseResult("var x = 5")(rootCtx)
    compile(parsed, initState)
    assertEquals("var x: Int = 5\n", stripColor(storedOutput()))
  }

  @Test def testRes = {
    implicit val ctx = rootCtx
    val parsed @ Parsed(_,_) = ParseResult(
      """|def foo = 1 + 1
         |val x = 5 + 5
         |1 + 1
         |var y = 5
         |10 + 10""".stripMargin
    )

    compile(parsed, initState)


    val expected = Set("def foo: Int",
                       "val x: Int = 10",
                       "val res1: Int = 20",
                       "val res0: Int = 2",
                       "var y: Int = 5")

    val actual = storedOutput()
    expected === stripColor(actual).split("\n")
  }

  @Test def testImportMutable = {
    val parsedImport @ Parsed(_,_) = ParseResult("import scala.collection.mutable")(rootCtx)
    val newState = compile(parsedImport, initState)

    assert(newState.imports.nonEmpty, "Didn't add import to `State` after compilation")

    val mutableUse @ Parsed(_,_) = ParseResult("""mutable.Map("one" -> 1)""")(rootCtx)

    compile(mutableUse, newState)

    assertEquals(
      "val res0: scala.collection.mutable.Map[String, Int] = Map(one -> 1)\n",
      stripColor(storedOutput())
    )
  }

  @Test def rebindVariable = {
    implicit val ctx = rootCtx
    val parsedImport @ Parsed(_,_) = ParseResult("var x = 5")
    val newState = compile(parsedImport, initState)

    val mutableUse @ Parsed(_,_) = ParseResult("x = 10")

    compile(mutableUse, newState)

    assertEquals(
      """|var x: Int = 5
         |x: Int = 10
         |""".stripMargin,
      stripColor(storedOutput())
    )
  }
}
