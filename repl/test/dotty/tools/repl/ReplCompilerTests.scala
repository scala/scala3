package dotty.tools
package repl

import scala.language.unsafeNulls

import java.util.regex.Pattern

import org.junit.Assert.{assertEquals, assertFalse, assertTrue}
import org.junit.Assert.{assertTrue => assert}
import org.junit.Test
import dotc.core.Contexts.Context

class ReplCompilerTests extends ReplTest:
  import ReplCompilerTests._

  private def lines() =
    storedOutput().trim.linesIterator.toList

  @Test def compileSingle = initially {
    run("def foo: 1 = 1")
    assertEquals("def foo: 1", storedOutput().trim)
  }

  @Test def i18383NoWarnOnUnusedImport: Unit = {
    initially {
      run("import scala.collection.*")
    } andThen {
      println(lines().mkString("* ", "\n  * ", ""))
    }
  }

  @Test def compileTwo =
    initially {
      run("def foo: 1 = 1")
    } andThen {
      val s2 = run("def foo(i: Int): i.type = i")
      assertEquals(2, s2.objectIndex)
    }

  @Test def inspectWrapper =
    initially {
      run("def foo = 1")
    } andThen {
      storedOutput() // discard output
      run("val x = rs$line$1.foo")
      assertEquals("val x: Int = 1", storedOutput().trim)
    }

  @Test def testVar = initially {
    run("var x = 5")
    assertEquals("var x: Int = 5", storedOutput().trim)
  }

  @Test def testRes = initially {
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
    initially {
      run("import scala.collection.mutable")
    } andThen {
      assertEquals(1, summon[State].imports.size)
      run("""mutable.Map("one" -> 1)""")
      assertEquals(
        "val res0: mutable.Map[String, Int] = HashMap(one -> 1)",
        storedOutput().trim
      )
    }

  @Test def rebindVariable =
    initially {
      val state = run("var x = 5")
      assertEquals("var x: Int = 5", storedOutput().trim)
      state
    } andThen {
      run("x = 10")
      assertEquals("x: Int = 10", storedOutput().trim)
    }

  @Test def defaultParameter = initially {
    run("def foo(a: Int = 1): 1 = 1")
    assertEquals("def foo(a: Int): 1", storedOutput().trim)
  }

  @Test def i8677 = initially {
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

  @Test def `i3305 SOE meh`: Unit = initially:
    run("def foo: Int = 1 + foo; foo")
    assert(storedOutput().startsWith("java.lang.StackOverflowError"))

  @Test def `i3305 NPE`: Unit = initially:
    run("null.toString")
    assert(storedOutput().startsWith("java.lang.NullPointerException"))

  @Test def `i3305 IAE`: Unit = initially:
    run("""throw new IllegalArgumentException("Hello")""")
    assertTrue(storedOutput().startsWith("java.lang.IllegalArgumentException: Hello"))

  @Test def `i3305 ME`: Unit = initially:
    run("val (x, y) = null")
    assert(storedOutput().startsWith("scala.MatchError: null"))

  @Test def i2789: Unit = initially {
    run("(x: Int) => println(x)")
    assert(storedOutput().startsWith("val res0: Int => Unit ="))
  }

  @Test def byNameParam: Unit = initially {
    run("def f(g: => Int): Int = g")
    assert(storedOutput().startsWith("def f(g: => Int): Int"))
  }

  @Test def i4051 = initially {
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
    initially {
      run("given Int = 10")
    } andThen {
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

  @Test def i16596 =
    initially {
      run("""
        |import scala.compiletime.{error, ops, requireConst}, ops.int.*
        |import scala.quoted.*
        |
        |sealed trait Nat
        |object Nat:
        |  case object Zero extends Nat
        |  case class Succ[N <: Nat](prev: N) extends Nat
        |
        |  given zero: Zero.type = Zero
        |  given buildSucc[N <: Nat](using n: N): Succ[N] = Succ(n)
        |
        |  def value[N <: Nat](using n: N): N = n
        |
        |  def prevImpl[I <: Int: Type](expr: Expr[I])(using Quotes): Expr[I - 1] =
        |    val prev = expr.valueOrAbort - 1
        |    // this compiles, but fails at use time
        |    //Expr(prev).asExprOf[ops.int.-[I, 1]]
        |    Expr(prev).asInstanceOf[Expr[I - 1]]
        |
        |  inline def prevOf[I <: Int](inline i: I): I - 1 = ${prevImpl('i)}
        |
        |  type FromInt[I <: Int] <: Nat = I match
        |     case 0 => Zero.type
        |     case _ => Succ[FromInt[I - 1]]
        |
        |  inline def fromInt[I <: Int & Singleton](i: I): FromInt[i.type] =
        |    requireConst(i)
        |    inline i match
        |      case _: 0 => Zero
        |      case _ =>
        |        inline if i < 0
        |        then error("cannot convert negative to Nat")
        |        else Succ(fromInt(prevOf[i.type](i)))
      """.stripMargin)
    }.andThen {
      assertMultiLineEquals(
        """// defined trait Nat
          |// defined object Nat
          |""".stripMargin, storedOutput())
      run("Nat.fromInt(2)")
    }.andThen {
      assertEquals("val res0: Nat.Succ[Nat.Succ[Nat.Zero.type]] = Succ(Succ(Zero))", storedOutput().trim)
      run("summon[Nat.FromInt[2]]")
    }.andThen {
      assertEquals("val res1: Nat.Succ[Nat.Succ[Nat.Zero.type]] = Succ(Succ(Zero))", storedOutput().trim)
      run("Nat.fromInt(3)")
    }.andThen {
      assertEquals("val res2: Nat.Succ[Nat.Succ[Nat.Succ[Nat.Zero.type]]] = Succ(Succ(Succ(Zero)))", storedOutput().trim)
      run("summon[Nat.FromInt[3]]")
    }.andThen {
      assertEquals("val res3: Nat.Succ[Nat.Succ[Nat.Succ[Nat.Zero.type]]] = Succ(Succ(Succ(Zero)))", storedOutput().trim)
    }

  @Test def i6200 =
    initially {
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
      """.stripMargin)
    } andThen {
      assertMultiLineEquals(
        """// defined trait Ord
          |// defined object IntOrd""".stripMargin,
        storedOutput().trim
      )
      run("IntOrd")
      assert(storedOutput().startsWith("val res0: IntOrd.type ="))
    }

  @Test def i7934: Unit = contextually {
    assertFalse(ParseResult.isIncomplete("_ + 1"))  // was: assertThrows[NullPointerException]
  }

  @Test def `i9374 accept collective extensions`: Unit = contextually {
    assert(ParseResult.isIncomplete("extension (x: String)"))
    assert(ParseResult.isIncomplete("extension (x: String) {"))
  }

  @Test def testSingletonPrint = initially {
    run("""val a = "hello"; val x: a.type = a""")
    assertMultiLineEquals("val a: String = \"hello\"\nval x: a.type = \"hello\"", storedOutput().trim)
  }

  @Test def i6574 = initially {
    run("val a: 1 | 0 = 1")
    assertEquals("val a: 1 | 0 = 1", storedOutput().trim)
  }

  @Test def `i10214 must show classic MatchError` = initially {
    run("val 1 = 2: @unchecked")
    assertEquals("scala.MatchError: 2 (of class java.lang.Integer)", storedOutput().linesIterator.next())
  }
  @Test def `i10214 must show useful regex MatchError` =
    initially {
      run("""val r = raw"\d+".r""")
    } andThen {
      run("""val r() = "abc": @unchecked""")
      assertEquals("scala.MatchError: abc (of class java.lang.String)", storedOutput().linesIterator.drop(1).next())
    }
  @Test def `i10214 must show MatchError on literal type` = initially {
    run("val (x: 1) = 2: @unchecked")
    assertEquals("scala.MatchError: 2 (of class java.lang.Integer)", storedOutput().linesIterator.next())
  }
  @Test def `i12920 must truncate stack trace to user code` = initially {
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
  @Test def `i14281 context class loader must be REPL class loader` = initially {
    run("class C ; assert(classOf[C].getClassLoader eq Thread.currentThread.getContextClassLoader)")
    assertEquals(List("// defined class C"), lines())
  }

  def assertNotFoundError(id: String): Unit =
    val lines = storedOutput().linesIterator
    assert(lines.next().startsWith("-- [E006] Not Found Error:"))
    assert(lines.drop(2).next().trim().endsWith(s"Not found: $id"))

  @Test def i4416 = initially {
    val state = run("val x = 1 / 0")
    val all = lines()
    assertEquals(2, all.length)
    assert(all.head.startsWith("java.lang.ArithmeticException:"))
    state
  } andThen {
    val state = run("def foo = x")
    assertNotFoundError("x")
    state
  } andThen {
    run("x")
    assertNotFoundError("x")
  }

  @Test def i4416b = initially {
    val state = run("val a = 1234")
    val _ = storedOutput() // discard output
    state
  } andThen {
    val state = run("val a = 1; val x = ???; val y = x")
    val all = lines()
    assertEquals(3, all.length)
    assertEquals("scala.NotImplementedError: an implementation is missing", all.head)
    state
  } andThen {
    val state = run("x")
    assertNotFoundError("x")
    state
  } andThen {
    val state = run("y")
    assertNotFoundError("y")
    state
  } andThen {
    run("a")   // `a` should retain its original binding
    assertEquals("val res0: Int = 1234", storedOutput().trim)
  }

  @Test def i4416_imports = initially {
    run("import scala.collection.mutable")
  } andThen {
    val state = run("import scala.util.Try; val x = ???")
    val _ = storedOutput() // discard output
    state
  } andThen {
    run(":imports")  // scala.util.Try should not be imported
    assertEquals("import scala.collection.mutable", storedOutput().trim)
  }

  @Test def i4416_types_defs_aliases = initially {
    val state =
      run("""|type Foo = String
             |trait Bar
             |def bar: Bar = ???
             |val x = ???
             |""".stripMargin)
    val all = lines()
    assertEquals(3, all.length)
    assertEquals("scala.NotImplementedError: an implementation is missing", all.head)
    assert("type alias in failed wrapper should not be rendered",
      !all.exists(_.startsWith("// defined alias type Foo = String")))
    assert("type definitions in failed wrapper should not be rendered",
      !all.exists(_.startsWith("// defined trait Bar")))
    assert("defs in failed wrapper should not be rendered",
      !all.exists(_.startsWith("def bar: Bar")))
    state
  } andThen {
    val state = run("def foo: Foo = ???")
    assertNotFoundError("type Foo")
    state
  } andThen {
    val state = run("type B = Bar")
    assertNotFoundError("type Bar")
    state
  } andThen {
    run("bar")
    assertNotFoundError("bar")
  }

  @Test def i14473 = initially {
    run("""val (x,y) = (if true then "hi" else (42,17)): @unchecked""")
    val all = lines()
    assertEquals(2, all.length)
    assertEquals("scala.MatchError: hi (of class java.lang.String)", all.head)
  }

  @Test def i14701 = initially {
    val state = run("val _ = ???")
    val all = lines()
    assertEquals(3, all.length)
    assertEquals("scala.NotImplementedError: an implementation is missing", all.head)
    state
  } andThen {
    run("val _ = assert(false)")
    val all = lines()
    assertEquals(3, all.length)
    assertEquals("java.lang.AssertionError: assertion failed", all.head)
  }

  @Test def `i13097 expect lambda after colon` = contextually:
    assert(ParseResult.isIncomplete("val x = List(42).foreach:"))

  @Test def `i13097 expect template after colon` = contextually:
    assert(ParseResult.isIncomplete("class C:"))

  @Test def i15562: Unit = initially {
    val s1 = run("List(1, 2).filter(_ % 2 == 0).foreach(println)")
    assertEquals("2", storedOutput().trim)
    s1
  } andThen { s1 ?=>
    val comp = tabComplete("List(1, 2).filter(_ % 2 == 0).fore")
    assertEquals(List("foreach"), comp.distinct)
    s1
  } andThen {
    val s2 = run("List(1, 2).filter(_ % 2 == 0).foreach(println)")
    assertEquals("2", storedOutput().trim)
    s2
  }

  @Test def i15562b: Unit = initially {
    val s1 = run("List(1, 2).filter(_ % 2 == 0).foreach(println)")
    assertEquals("2", storedOutput().trim)
    s1
  } andThen { s1 ?=>
    val comp = tabComplete("val x = false + true; List(1, 2).filter(_ % 2 == 0).fore")
    assertEquals(List("foreach"), comp.distinct)
    s1
  } andThen {
    val s2 = run("List(1, 2).filter(_ % 2 == 0).foreach(println)")
    assertEquals("2", storedOutput().trim)
    s2
  }

  @Test def `i17333 print null result of toString`: Unit =
    initially:
      run("val tpolecat = new Object { override def toString(): String = null }")
    .andThen:
      val last = lines().last
      assertTrue(last, last.startsWith("val tpolecat: Object = null"))

  @Test def `i17333 print toplevel object with null toString`: Unit =
    initially:
      run("object tpolecat { override def toString(): String = null }")
    .andThen:
      run("tpolecat")
      val last = lines().last
      assertTrue(last, last.startsWith("val res0: tpolecat.type = null"))

  @Test def `i21431 filter out best effort options`: Unit =
    initially:
      run(":settings -Ybest-effort -Ywith-best-effort-tasty")
    .andThen:
      run("0") // check for crash
      val last = lines()
      assertTrue(last(0), last(0) == ("Options incompatible with repl will be ignored: -Ybest-effort, -Ywith-best-effort-tasty"))
      assertTrue(last(1), last(1) == ("val res0: Int = 0"))

  @Test def `i9879`: Unit = initially:
    run {
      """|opaque type A = Int; def getA: A = 0
         |object Wrapper { opaque type A = Int; def getA: A = 1 }
         |val x = getA
         |val y = Wrapper.getA""".stripMargin
    }
    val expected = List(
      "def getA: A",
      "// defined object Wrapper",
      "val x: A = 0",
      "val y: Wrapper.A = 1"
    )
    assertEquals(expected, lines())

  @Test def `i9879b`: Unit = initially:
    run {
      """|def test =
         |  type A = Int
         |  opaque type B = String
         |  object Wrapper { opaque type C = Int }
         |  ()""".stripMargin
    }
    val all = lines()
    assertEquals(6, all.length)
    assertTrue(all.head.startsWith("-- [E103] Syntax Error"))
    assertTrue(all.exists(_.trim().startsWith("|  Illegal start of statement: this modifier is not allowed here")))

  @Test def `i16250a`: Unit = initially:
    val hints = List(
      "this language import is not allowed in the REPL",
      "To use this language feature, include the flag `-language:experimental.captureChecking` when starting the REPL"
    )
    run("import language.experimental.captureChecking")
    val all = lines()
    assertTrue(hints.forall(hint => all.exists(_.contains(hint))))

  @Test def `i16250b`: Unit = initially:
    val hints = List(
      "this language import is not allowed in the REPL",
      "To use this language feature, include the flag `-language:experimental.pureFunctions` when starting the REPL"
    )
    run("import language.experimental.pureFunctions")
    val all = lines()
    assertTrue(hints.forall(hint => all.exists(_.contains(hint))))

  @Test def `i22844 regression colon eol`: Unit = initially:
    run:
      """|println:
         |  "hello, world"
         |""".stripMargin // outdent, but this test does not exercise the bug
    assertEquals(List("hello, world"), lines())

  @Test def `i22844b regression colon arrow eol`: Unit = contextually:
    assertTrue(ParseResult.isIncomplete("List(42).map: x =>"))

  // i24142: Colon operator at line beginning should be treated as Scala code, not REPL command
  @Test def `i24142 cons operator`: Unit = initially:
    run("::(1, Nil)")
    assertEquals("val res0: ::[Int] = List(1)", storedOutput().trim)

  @Test def `i24142 symbolic identifier starting with colon`: Unit = initially:
    // Test a custom identifier starting with :
    run("val `:test` = 42")
    assertEquals("val :test: Int = 42", storedOutput().trim)

  @Test def `i24142 colon as infix`: Unit = initially:
    run("1 :: 2 :: Nil")
    assertEquals("val res0: List[Int] = List(1, 2)", storedOutput().trim)

  @Test def `i24142 commands still work`: Unit = initially:
    run(":help")
    assertTrue(storedOutput().contains("The REPL has several commands available"))

  @Test def `i24142 abbreviated commands still work`: Unit = initially:
    run(":he")
    assertTrue(storedOutput().contains("The REPL has several commands available"))

object ReplCompilerTests:

  private val pattern = Pattern.compile("\\r[\\n]?|\\n");

  // Ensure 'expected' and 'actual' contain the same line separator(s).
  def assertMultiLineEquals(expected: String, actual: String): Unit =
    val expected0 = pattern.matcher(expected).replaceAll(System.lineSeparator)
    val actual0 = pattern.matcher(actual).replaceAll(System.lineSeparator)
    assertEquals(expected0, actual0)

end ReplCompilerTests

class ReplXPrintTyperTests extends ReplTest(ReplTest.defaultOptions :+ "-Vprint:typer"):
  @Test def i9111 = initially {
    run("""|enum E {
           |  case A
           |}""".stripMargin)
    assert(storedOutput().trim().endsWith("// defined class E"))
  }

  @Test def i10883 = initially {
    run("val a = 42")
    assert(storedOutput().trim().endsWith("val a: Int = 42"))
  }
end ReplXPrintTyperTests

class ReplVerboseTests extends ReplTest(ReplTest.defaultOptions :+ "-verbose"):
  @Test def i9111 = initially {
    run("""|enum E {
           |  case A
           |}""".stripMargin)
    assert(storedOutput().trim().endsWith("// defined class E"))
  }

  @Test def i10883 = initially {
    run("val a = 42")
    assert(storedOutput().trim().endsWith("val a: Int = 42"))
  }

  @Test def `i4393-incomplete-catch`: Unit = contextually {
    assert(ParseResult.isIncomplete("""|try {
                                       |  ???
                                       |} catch""".stripMargin))
    assert(ParseResult.isIncomplete("""|try {
                                       |  ???
                                       |} catch {""".stripMargin))
  }

end ReplVerboseTests

class ReplHighlightTests extends ReplTest(ReplTest.defaultOptions.filterNot(_.startsWith("-color")) :+ "-color:always"):
  @Test def i18596: Unit = initially:
    run("""(1 to 500).foldRight("x") { case (_, n) => s"<x>$n</x>" }""")

  @Test def i16904: Unit = initially:
    run(""""works not fine"* 10000""")

    run("""
      case class Tree(left: Tree, right: Tree)
      def deepTree(depth: Int): Tree
      deepTree(300)""")

class ReplUnrollTests extends ReplTest(ReplTest.defaultOptions ++ Seq("-experimental", "-Xprint:pickler")):
  override val redirectOutput = true
  @Test def i23408: Unit = initially:
    run("""
      import scala.annotation.unroll
      case class Foo(x: Int, @unroll y: Option[String] = None)"""
    )
    val expected = List(
      "def copy(x: Int, y: Option[String]): Foo = new Foo(x, y)",
      "def copy(x: Int): Foo = this.copy(x, this.copy$default$2)",
      "def copy$default$1: Int @uncheckedVariance = Foo.this.x",
      "def copy$default$2: Option[String] @uncheckedVariance = Foo.this.y",
      "def apply(x: Int, y: Option[String]): Foo = new Foo(x, y)",
      "def apply(x: Int): Foo = this.apply(x, Foo.$lessinit$greater$default$2)",
      """def fromProduct(x$0: Product): Foo.MirroredMonoType = {
          val arity: Int = x$0.productArity
          val x$1: Int = x$0.productElement(0).$asInstanceOf[Int]
          val y$1: Option[String] = (if arity > 1 then x$0.productElement(1) else Foo.$lessinit$greater$default$2).$asInstanceOf[Option[String]]
          new Foo(x$1, y$1)
        }"""
    )
    def trimWhitespaces(input: String): String = input.replaceAll("\\s+", " ")
    val output = storedOutput()
    val normalizedOutput = trimWhitespaces(output)
    expected.foreach: defn =>
      val normalizedDefn = trimWhitespaces(defn)
      assertTrue(
        s"Output: '$output' did not contain expected definition: ${defn}",
        normalizedOutput.contains(normalizedDefn)
      )
