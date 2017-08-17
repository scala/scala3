package dotty.tools
package repl

import org.junit.Assert._
import org.junit.Test

import dotc.core.Contexts.Context
import dotc.reporting.MessageRendering
import dotc.ast.untpd

import results._
import ReplTest._

class ReplCompilerTests extends ReplTest with MessageRendering {

  @Test def compileSingle = withState {
    compiler.compile("def foo: 1 = 1").fold(onErrors, _ => ())
  }

  @Test def compileTwo = withState {
    val secondState =
      for {
        (_, s1) <- compiler.compile("def foo: 1 = 1")
        (_, s2) <- fromState(s1) {
          compiler.compile("def foo(i: Int): i.type = i")
        }
      } yield s2

    secondState.fold(onErrors, { state =>
      assert(state.objectIndex == 2,
             s"Wrong object offset: expected 2 got ${state.objectIndex}")
    })
  }

  @Test def inspectSingle = withState {
    val untpdTree = compiler.compile("def foo: 1 = 1").map((u,_) => u.untpdTree)

    untpdTree.fold(
      onErrors,
      _ match {
        case untpd.PackageDef(_, List(mod: untpd.ModuleDef)) =>
          assert(mod.name.show == "rs$line$1", mod.name.show)
        case tree => fail(s"Unexpected structure: $tree")
      }
    )
  }

  @Test def testVar = withState {
    compile("var x = 5")
    assertEquals("var x: Int = 5\n", stripColor(storedOutput()))
  }

  @Test def testRes = withState {
    compile {
      """|def foo = 1 + 1
         |val x = 5 + 5
         |1 + 1
         |var y = 5
         |10 + 10""".stripMargin
    }

    val expected = Set("def foo: Int",
                       "val x: Int = 10",
                       "val res1: Int = 20",
                       "val res0: Int = 2",
                       "var y: Int = 5")

    val actual = storedOutput()
    expected === stripColor(actual).split("\n")
  }

  @Test def testImportMutable = withState {
    fromState(compile("import scala.collection.mutable")) {
      assert(implicitly[State].imports.nonEmpty, "Didn't add import to `State` after compilation")

      compile("""mutable.Map("one" -> 1)""")

      assertEquals(
        "val res0: scala.collection.mutable.Map[String, Int] = Map(one -> 1)\n",
        stripColor(storedOutput())
      )
    }
  }

  @Test def rebindVariable = withState {
    fromState(compile("var x = 5")) {
      compile("x = 10")
      assertEquals(
        """|var x: Int = 5
           |x: Int = 10
           |""".stripMargin,
        stripColor(storedOutput())
      )
    }
  }
}
