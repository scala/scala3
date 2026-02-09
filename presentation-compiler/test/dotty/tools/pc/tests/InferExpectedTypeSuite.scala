package dotty.tools.pc.tests

import java.nio.file.Paths

import scala.language.unsafeNulls
import scala.meta.internal.metals.CompilerOffsetParams
import scala.meta.internal.metals.EmptyCancelToken
import scala.meta.internal.mtags.CommonMtagsEnrichments.*

import dotty.tools.pc.ScalaPresentationCompiler
import dotty.tools.pc.base.BasePCSuite

import org.junit.Ignore
import org.junit.Test

class InferExpectedTypeSuite extends BasePCSuite:
  def check(
      original: String,
      expectedType: String,
      fileName: String = "A.scala"
  ): Unit =
    presentationCompiler.restart()
    val (code, offset) = params(original.replace("@@", "CURSOR@@"), fileName)
    val offsetParams = CompilerOffsetParams(
      Paths.get(fileName).toUri(),
      code,
      offset,
      EmptyCancelToken
    )
    presentationCompiler.asInstanceOf[ScalaPresentationCompiler].inferExpectedType(offsetParams).get().asScala match
      case Some(value) => assertNoDiff(expectedType, value)
      case None => fail("Empty result.")

  @Test def basic =
    check(
      """|def doo: Double = @@
         |""".stripMargin,
      """|Double
         |""".stripMargin
    )

  @Test def `basic-param` =
    check(
      """|def paint(c: Int) = ???
         |val _ = paint(@@)
         |""".stripMargin,
      """|Int
         |""".stripMargin
    )

  @Test def `basic-params` =
    check(
      """|def paint(c: Int, f: String, d: List[String]) = ???
         |val _ = paint(1, "aa", @@)
         |""".stripMargin,
      """|List[String]
         |""".stripMargin
    )

  @Test def `basic-type-param` =
    check(
      """|def paint[T](c: T) = ???
         |val _ = paint[Int](@@)
         |""".stripMargin,
      """|Int
         |""".stripMargin
    )

  @Test def `type-ascription` =
    check(
      """|def doo = (@@ : Double)
         |""".stripMargin,
      """|Double
         |""".stripMargin
    )

  @Test def list =
    check(
      """|val i: List[Int] = List(@@)
         |""".stripMargin,
      """|Int
         |""".stripMargin
    )

  @Test def `list-singleton` =
    check(
      """|val i: List["foo"] = List("@@")
         |""".stripMargin,
      """|"foo"
         |""".stripMargin
    )

  @Test def option =
    check(
      """|val i: Option[Int] = Option(@@)
         |""".stripMargin,
      """|Int | Null
         |""".stripMargin
    )

// some structures
  @Test def `with-block` =
    check(
      """|def c: Double =
         |  @@
         |""".stripMargin,
      """|Double
         |""".stripMargin
    )

  @Test def `if-statement` =
    check(
      """|def c(shouldBeBlue: Boolean): Int =
         |  if(shouldBeBlue) @@
         |  else 2
         |""".stripMargin,
      """|Int
         |""".stripMargin
    )

  @Test def `if-statement-2` =
    check(
      """|def c(shouldBeBlue: Boolean): Int =
         |  if(shouldBeBlue) 1
         |  else @@
         |""".stripMargin,
      """|Int
         |""".stripMargin
    )

  @Test def `if-statement-3` =
    check(
      """|def c(shouldBeBlue: Boolean): Int =
         |  if(@@) 3
         |  else 2
         |""".stripMargin,
      """|Boolean
         |""".stripMargin
    )

  @Test def `try` =
    check(
      """|val _: Int =
         |  try {
         |    @@
         |  } catch {
         |    case _ =>
         |  }
         |""".stripMargin,
      """|Int
         |""".stripMargin
    )

  @Test def `try-catch` =
    check(
      """|val _: Int =
         |  try {
         |  } catch {
         |    case _ => @@
         |  }
         |""".stripMargin,
      """|Int
         |""".stripMargin
    )

  @Test def `if-condition` =
    check(
      """|val _ = if @@ then 1 else 2
         |""".stripMargin,
      """|Boolean
         |""".stripMargin
    )

  @Test def `inline-if` =
    check(
      """|inline def o: Int = inline if ??? then @@ else ???
         |""".stripMargin,
      """|Int
         |""".stripMargin
    )

// pattern matching

  @Test def `pattern-match` =
    check(
      """|val _ =
         |  List(1) match
         |    case @@
         |""".stripMargin,
      """|List[Int]
         |""".stripMargin
    )

  @Test def bind =
    check(
      """|val _ =
         |  List(1) match
         |    case name @ @@
         |""".stripMargin,
      """|List[Int]
         |""".stripMargin
    )

  @Test def alternative =
    check(
      """|val _ =
         |  List(1) match
         |    case Nil | @@
         |""".stripMargin,
      """|List[Int]
         |""".stripMargin
    )

  @Test def unapply =
    check(
      """|val _ =
         |  List(1) match
         |    case @@ :: _ =>
         |""".stripMargin,
      """|Int
         |""".stripMargin
    )

// generic functions

  @Test def `any-generic` =
    check(
      """|val _ : List[Int] = identity(@@)
         |""".stripMargin,
      """|List[Int]
         |""".stripMargin
    )

  @Test def `eq-generic` =
    check(
      """|def eq[T](a: T, b: T): Boolean = ???
         |val _ = eq(1, @@)
         |""".stripMargin,
      """|Int
         |""".stripMargin
    )

  @Test def flatmap =
    check(
      """|val _ : List[Int] = List().flatMap(_ => @@)
         |""".stripMargin,
      """|IterableOnce[Nothing]
         |""".stripMargin // ideally IterableOnce[Int], but can't change interpolateTypeVars
    )

  @Test def map =
    check(
      """|val _ : List[Int] = List().map(_ => @@)
         |""".stripMargin,
      """|Nothing
         |""".stripMargin // ideally Int, but can't change interpolateTypeVars
    )

  @Test def `for-comprehension` =
    check(
      """|val _ : List[Int] =
         |  for {
         |    _ <- List("a", "b")
         |  } yield @@
         |""".stripMargin,
      """|Nothing
         |""".stripMargin // ideally Int, but can't change interpolateTypeVars
    )

// bounds
  @Test def any =
    check(
      """|trait Foo
         |def foo[T](a: T): Boolean = ???
         |val _ = foo(@@)
         |""".stripMargin,
      """|Any
         |""".stripMargin
    )

  @Test def `bounds-1` =
    check(
      """|trait Foo
         |def foo[T <: Foo](a: T): Boolean = ???
         |val _ = foo(@@)
         |""".stripMargin,
      """|Foo
         |""".stripMargin
    )

  @Test def `bounds-2` =
    check(
      """|trait Foo
         |def foo[T >: Foo](a: T): Boolean = ???
         |val _ = foo(@@)
         |""".stripMargin,
      """|Foo
         |""".stripMargin // ideally Any (maybe?)
    )

  @Test def `bounds-3` =
    check(
      """|trait A
         |class B extends A
         |class C extends B
         |def roo[F >: C <: A](f: F) = ???
         |val kjk = roo(@@)
         |""".stripMargin,
      """|C
         |""".stripMargin // ideally A
    )

  @Test def `multiple-args-lists` =
    check(
      """|def m(i: Int)(s: String) = ???
         |val x = m(@@)
         |""".stripMargin,
      """|Int
         |""".stripMargin
    )

  @Test def `multiple-args-lists-2` =
    check(
      """|def m(i: Int)(s: String) = ???
         |val x = m(1)(@@)
         |""".stripMargin,
      """|String
        |""".stripMargin
    )

  @Test def `extension-methods` =
    check(
      """|extension (i: Int) {
         |  def method(s: String): Unit = ()
         |}
         |
         |def testIt =
         |  7.method(@@)
         |""".stripMargin,
      """|String
         |""".stripMargin
    )

  @Test def `implicit-methods` =
    check(
      """|object I
         |implicit class Xtension(i: I.type) {
         |  def method(s: String): Unit = ()
         |}
         |
         |def testIt =
         |  I.method(@@)
         |""".stripMargin,
      """|String
         |""".stripMargin
    )

  @Test def using =
    check(
      """|def go(using Ordering[Int])(x: Int, y: Int): Int =
         |  Ordering[Int].compare(x, y)
         |
         |def test =
         |  go(???, @@)
         |""".stripMargin,
      """|Int
         |""".stripMargin
    )

  @Test def `apply-dynamic` =
    check(
      """|object TypedHoleApplyDynamic {
         |  val obj: reflect.Selectable {
         |    def method(x: Int): Unit
         |  } = new reflect.Selectable {
         |    def method(x: Int): Unit = ()
         |  }
         |
         |  obj.method(@@)
         |}
         |""".stripMargin,
      "Int"
    )

  @Test def `apply-dynamic-2` =
    check(
      """|object TypedHoleApplyDynamic {
         |  val obj: reflect.Selectable {
         |    def method[T](x: Int, y: T): Unit
         |  } = new reflect.Selectable {
         |    def method[T](x: Int, y: T): Unit = ()
         |  }
         |
         |  obj.method[String](1, @@)
         |}
         |""".stripMargin,
      "String"
    )

  @Test def `apply-dynamic-3` =
    check(
      """|object TypedHoleApplyDynamic {
         |  val obj: reflect.Selectable {
         |    def method[T](a: Int)(x: Int, y: T): Unit
         |  } = new reflect.Selectable {
         |    def method[T](a: Int)(x: Int, y: T): Unit = ()
         |  }
         |
         |  obj.method[String](1)(1, @@)
         |}
         |""".stripMargin,
      "String"
    )
