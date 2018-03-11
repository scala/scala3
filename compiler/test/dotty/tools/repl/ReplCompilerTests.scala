package dotty.tools
package repl

import org.junit.Assert._
import org.junit.{Test, Ignore}

import dotc.core.Contexts.Context
import dotc.ast.Trees._
import dotc.ast.untpd

import results._
import ReplTest._

class ReplCompilerTests extends ReplTest {

  @Test def compileSingle: Unit = fromInitialState { implicit state =>
    compiler.compile("def foo: 1 = 1".toParsed).stateOrFail
  }


  @Test def compileTwo =
    fromInitialState { implicit state =>
      compiler.compile("def foo: 1 = 1".toParsed).stateOrFail
    }
    .andThen { implicit state =>
      val s2 = compiler.compile("def foo(i: Int): i.type = i".toParsed).stateOrFail
      assert(s2.objectIndex == 2,
             s"Wrong object offset: expected 2 got ${s2.objectIndex}")
    }

  @Test def inspectSingle =
    fromInitialState { implicit state =>
      val untpdTree = compiler.compile("def foo: 1 = 1".toParsed).map(_._1.untpdTree)

      untpdTree.fold(
        onErrors,
        _ match {
          case PackageDef(_, List(mod: untpd.ModuleDef)) =>
            implicit val ctx = state.run.runContext
            assert(mod.name.show == "rs$line$1", mod.name.show)
          case tree => fail(s"Unexpected structure: $tree")
        }
      )
    }

  @Test def testVar = fromInitialState { implicit state =>
    compile("var x = 5".toParsed)
    assertEquals("var x: Int = 5\n", storedOutput())
  }

  @Test def testRes = fromInitialState { implicit state =>
    compile {
      """|def foo = 1 + 1
         |val x = 5 + 5
         |1 + 1
         |var y = 5
         |10 + 10""".stripMargin.toParsed
    }

    val expected = Set("def foo: Int",
                       "val x: Int = 10",
                       "val res1: Int = 20",
                       "val res0: Int = 2",
                       "var y: Int = 5")

    expected === storedOutput().split("\n")
  }

  @Test def testImportMutable =
    fromInitialState { implicit state =>
      compile("import scala.collection.mutable".toParsed)
    }
    .andThen { implicit state =>
      assert(state.imports.nonEmpty, "Didn't add import to `State` after compilation")

      compile("""mutable.Map("one" -> 1)""".toParsed)

      assertEquals(
        "val res0: scala.collection.mutable.Map[String, Int] = Map(one -> 1)\n",
        storedOutput()
      )
    }

  @Test def rebindVariable =
    fromInitialState { implicit s => compile("var x = 5".toParsed) }
    .andThen { implicit s =>
      compile("x = 10".toParsed)
      assertEquals(
        """|var x: Int = 5
           |x: Int = 10
           |""".stripMargin,
        storedOutput()
      )
    }

  // FIXME: Tests are not run in isolation, the classloader is corrupted after the first exception
  @Ignore def i3305: Unit = {
    fromInitialState { implicit s =>
      compile("null.toString".toParsed)
      assertTrue(storedOutput().startsWith("java.lang.NullPointerException"))
    }

    fromInitialState { implicit s =>
      compile("def foo: Int = 1 + foo; foo".toParsed)
      assertTrue(storedOutput().startsWith("def foo: Int\njava.lang.StackOverflowError"))
    }

    fromInitialState { implicit s =>
      compile("""throw new IllegalArgumentException("Hello")""".toParsed)
      assertTrue(storedOutput().startsWith("java.lang.IllegalArgumentException: Hello"))
    }

    fromInitialState { implicit s =>
      compile("val (x, y) = null".toParsed)
      assertTrue(storedOutput().startsWith("scala.MatchError: null"))
    }
  }

  @Test def i2789: Unit = fromInitialState { implicit state =>
    compile("(x: Int) => println(x)".toParsed)
    assertTrue(storedOutput().startsWith("val res0: Int => Unit ="))
  }
}
