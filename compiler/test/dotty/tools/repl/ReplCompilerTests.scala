package dotty.tools.repl

import java.util.regex.Pattern

import org.junit.Assert.{assertTrue => assert, _}
import org.junit.{Ignore, Test}
import dotty.tools.dotc.core.Contexts.Context

class ReplCompilerTests extends ReplTest {
  import ReplCompilerTests._

  private def lines() =
    storedOutput().trim.linesIterator.toList

  @Test def compileSingle = fromInitialState { implicit state =>
    run("def foo: 1 = 1")
    assertEquals("def foo: 1", storedOutput().trim)
  }

  @Test def compileTwo =
    fromInitialState { implicit state =>
      run("def foo: 1 = 1")
    }
    .andThen { implicit state =>
      val s2 = run("def foo(i: Int): i.type = i")
      assertEquals(2, s2.objectIndex)
    }

  @Test def inspectWrapper =
    fromInitialState { implicit state =>
      run("def foo = 1")

    }.andThen { implicit state =>
      storedOutput() // discard output
      run("val x = rs$line$1.foo")
      assertEquals("val x: Int = 1", storedOutput().trim)
    }

  @Test def testVar = fromInitialState { implicit state =>
    run("var x = 5")
    assertEquals("var x: Int = 5", storedOutput().trim)
  }

  @Test def testRes = fromInitialState { implicit state =>
    run {
      """|def foo = 1 + 1
         |val x = 5 + 5
         |1 + 1
         |var y = 5
         |10 + 10""".stripMargin
    }

    val expected = List(
      "def foo: Int",
      "val x: Int = 10",
      "val res0: Int = 2",
      "var y: Int = 5",
      "val res1: Int = 20"
    )

    assertEquals(expected, lines())
  }

  @Test def testImportMutable =
    fromInitialState { implicit state =>
      run("import scala.collection.mutable")
    }
    .andThen { implicit state =>
      assertEquals(1, state.imports.size)
      run("""mutable.Map("one" -> 1)""")
      assertEquals(
        "val res0: scala.collection.mutable.Map[String, Int] = HashMap(one -> 1)",
        storedOutput().trim
      )
    }

  @Test def rebindVariable =
    fromInitialState { implicit s =>
      val state = run("var x = 5")
      assertEquals("var x: Int = 5", storedOutput().trim)
      state
    }
    .andThen { implicit s =>
      run("x = 10")
      assertEquals("x: Int = 10", storedOutput().trim)
    }

  @Test def defaultParamter = fromInitialState { implicit state =>
    run("def foo(a: Int = 1): 1 = 1")
    assertEquals("def foo(a: Int): 1", storedOutput().trim)
  }

  @Test def i8677 = fromInitialState { implicit state =>
    run {
      """|sealed trait T1
         |case class X() extends T1
         |case class Y() extends T1
         |case object O extends T1
         """.stripMargin
    }

    val expected = List(
     "// defined trait T1",
     "// defined case class X",
     "// defined case class Y",
     "// defined case object O"
    )

    assertEquals(expected, lines())
  }

  // FIXME: Tests are not run in isolation, the classloader is corrupted after the first exception
  @Ignore @Test def i3305: Unit = {
    fromInitialState { implicit s =>
      run("null.toString")
      assert(storedOutput().startsWith("java.lang.NullPointerException"))
    }

    fromInitialState { implicit s =>
      run("def foo: Int = 1 + foo; foo")
      assert(storedOutput().startsWith("def foo: Int\njava.lang.StackOverflowError"))
    }

    fromInitialState { implicit s =>
      run("""throw new IllegalArgumentException("Hello")""")
      assert(storedOutput().startsWith("java.lang.IllegalArgumentException: Hello"))
    }

    fromInitialState { implicit s =>
      run("val (x, y) = null")
      assert(storedOutput().startsWith("scala.MatchError: null"))
    }
  }

  @Test def i2789: Unit = fromInitialState { implicit state =>
    run("(x: Int) => println(x)")
    assert(storedOutput().startsWith("val res0: Int => Unit ="))
  }

  @Test def byNameParam: Unit = fromInitialState { implicit state =>
    run("def f(g: => Int): Int = g")
    assert(storedOutput().startsWith("def f(g: => Int): Int"))
  }

  @Test def i4051 = fromInitialState { implicit state =>
    val source =
      """val x: PartialFunction[Int, Int] = { case x => x }
        |val y = Map(("A", 1), ("B", 2), ("X", 3)).collect { case (k, v) => v }.toList""".stripMargin

    val expected = List(
      "val x: PartialFunction[Int, Int] = <function1>",
      "val y: List[Int] = List(1, 2, 3)"
    )

    run(source)
    assertEquals(expected, lines())
  }

  @Test def i5897 =
    fromInitialState { implicit state => run("given Int = 10") }
    .andThen         { implicit state =>
      assertEquals(
        "lazy val given_Int: Int",
        storedOutput().trim
      )
      run("implicitly[Int]")
      assertEquals(
        "val res0: Int = 10",
        storedOutput().trim
      )
    }

  @Test def i6200 =
    fromInitialState { implicit state =>
      run("""
        |trait Ord[T] {
        |  def compare(x: T, y: T): Int
        |  extension (x: T) def < (y: T) = compare(x, y) < 0
        |  extension (x: T) def > (y: T) = compare(x, y) > 0
        |}
        |
        |given IntOrd: Ord[Int] with {
        |  def compare(x: Int, y: Int) =
        |  if (x < y) -1 else if (x > y) +1 else 0
        |}
      """.stripMargin) }
    .andThen         { implicit state =>
      assertMultiLineEquals(
        """// defined trait Ord
          |// defined object IntOrd""".stripMargin,
        storedOutput().trim
      )
      run("IntOrd")
      assert(storedOutput().startsWith("val res0: IntOrd.type ="))
    }

  @Test def i7934: Unit = fromInitialState { state =>
    given Context = state.context
    assertFalse(ParseResult.isIncomplete("_ + 1"))  // was: assertThrows[NullPointerException]
  }

  @Test def testSingletonPrint = fromInitialState { implicit state =>
    run("""val a = "hello"; val x: a.type = a""")
    assertMultiLineEquals("val a: String = hello\nval x: a.type = hello", storedOutput().trim)
  }

  @Test def i6574 = fromInitialState { implicit state =>
    run("val a: 1 | 0 = 1")
    assertEquals("val a: 1 | 0 = 1", storedOutput().trim)
  }

  @Test def `i10214 must show classic MatchError` = fromInitialState { implicit state =>
    run("val 1 = 2")
    assertEquals("scala.MatchError: 2 (of class java.lang.Integer)", storedOutput().linesIterator.next())
  }
  @Test def `i10214 must show useful regex MatchError` =
    fromInitialState { implicit state =>
      run("""val r = raw"\d+".r""")
    } andThen { implicit state =>
      run("""val r() = "abc"""")
      assertEquals("scala.MatchError: abc (of class java.lang.String)", storedOutput().linesIterator.drop(1).next())
    }
  @Test def `i10214 must show MatchError on literal type` = fromInitialState { implicit state =>
    run("val (x: 1) = 2")
    assertEquals("scala.MatchError: 2 (of class java.lang.Integer)", storedOutput().linesIterator.next())
  }
  @Test def `i12920 must truncate stack trace to user code` = fromInitialState { implicit state =>
    run("???")
    val all = lines()
    assertEquals(3, all.length)
    assertEquals("scala.NotImplementedError: an implementation is missing", all.head)
    /* avoid asserting much about line number or elided count
    scala.NotImplementedError: an implementation is missing
      at scala.Predef$.$qmark$qmark$qmark(Predef.scala:344)
      ... 28 elided
     */
  }
}

object ReplCompilerTests {

  private val pattern = Pattern.compile("\\r[\\n]?|\\n");

  // Ensure 'expected' and 'actual' contain the same line separator(s).
  def assertMultiLineEquals(expected: String, actual: String): Unit = {
    val expected0 = pattern.matcher(expected).replaceAll(System.lineSeparator)
    val actual0 = pattern.matcher(actual).replaceAll(System.lineSeparator)
    assertEquals(expected0, actual0)
  }

}

class ReplXPrintTyperTests extends ReplTest(ReplTest.defaultOptions :+ "-Xprint:typer") {
  @Test def i9111 = fromInitialState { implicit state =>
    run("""|enum E {
           |  case A
           |}""".stripMargin)
    assert(storedOutput().trim().endsWith("// defined class E"))
  }

  @Test def i10883 = fromInitialState { implicit state =>
    run("val a = 42")
    assert(storedOutput().trim().endsWith("val a: Int = 42"))
  }
}

class ReplVerboseTests extends ReplTest(ReplTest.defaultOptions :+ "-verbose") {
  @Test def i9111 = fromInitialState { implicit state =>
    run("""|enum E {
           |  case A
           |}""".stripMargin)
    assert(storedOutput().trim().endsWith("// defined class E"))
  }

  @Test def i10883 = fromInitialState { implicit state =>
    run("val a = 42")
    assert(storedOutput().trim().endsWith("val a: Int = 42"))
  }
}
