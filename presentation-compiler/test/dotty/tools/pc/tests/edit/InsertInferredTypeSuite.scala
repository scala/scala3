package dotty.tools.pc.tests.edit

import java.net.URI

import scala.language.unsafeNulls
import scala.meta.internal.jdk.CollectionConverters.*
import scala.meta.internal.metals.CompilerOffsetParams

import dotty.tools.pc.base.BaseCodeActionSuite
import dotty.tools.pc.utils.TextEdits

import org.eclipse.lsp4j as l
import org.junit.Test

class InsertInferredTypeSuite extends BaseCodeActionSuite:

  @Test def `val` =
    checkEdit(
      """|object A{
         |  val <<alpha>> = 123
         |}""".stripMargin,
      """|object A{
         |  val alpha: Int = 123
         |}""".stripMargin
    )

  @Test def `wrong-def-params` =
    checkEdit(
      """|object A{
         |  def <<alpha>>(a: Int, b: String): String = 123
         |}""".stripMargin,
      """|object A{
         |  def alpha(a: Int, b: String): Int = 123
         |}""".stripMargin
    )

  @Test def `wrong-val` =
    checkEdit(
      """|object A{
         |  val <<alpha>>:  String = 123
         |}""".stripMargin,
      """|object A{
         |  val alpha: Int = 123
         |}""".stripMargin
    )

  @Test def `wrong-val2` =
    checkEdit(
      """|object A{
         |  val <<alpha>> :  String = List(1, 2, 3)
         |}""".stripMargin,
      """|object A{
         |  val alpha: List[Int] = List(1, 2, 3)
         |}""".stripMargin
    )

  @Test def `wrong-val3` =
    checkEdit(
      """|object A{
         |  val <<alpha>> :  List[Int] = ""
         |}""".stripMargin,
      """|object A{
         |  val alpha: String = ""
         |}""".stripMargin
    )

  @Test def `wrong-val4` =
    checkEdit(
      """|object A{
         |  val <<alpha>> :  List[Int] = s""
         |}""".stripMargin,
      """|object A{
         |  val alpha: String = s""
         |}""".stripMargin
    )

  @Test def `wrong-def` =
    checkEdit(
      """|object A{
         |  def <<alpha>> :  String = 123
         |}""".stripMargin,
      """|object A{
         |  def alpha: Int = 123
         |}""".stripMargin
    )

  @Test def `wrong-def2` =
    checkEdit(
      """|object A{
         |  def <<alpha>> :  String = List(1, 2, 3)
         |}""".stripMargin,
      """|object A{
         |  def alpha: List[Int] = List(1, 2, 3)
         |}""".stripMargin
    )

  @Test def `wrong-def3` =
    checkEdit(
      """|object A{
         |  def <<alpha>> :  List[Int] = ""
         |}""".stripMargin,
      """|object A{
         |  def alpha: String = ""
         |}""".stripMargin
    )

  @Test def `wrong-def4` =
    checkEdit(
      """|object A{
         |  def <<alpha>> :  List[Int] = s""
         |}""".stripMargin,
      """|object A{
         |  def alpha: String = s""
         |}""".stripMargin
    )

  @Test def `wrong-def-toplevel` =
    checkEdit(
      """|def hello =
         |  val <<a>> :  List[Int] = ""
         |""".stripMargin,
      """|def hello =
         |  val a: String = ""
         |
         |""".stripMargin
    )

  @Test def `toplevel` =
    checkEdit(
      """|def <<alpha>> = List("")
         |""".stripMargin,
      """|def alpha: List[String] = List("")
         |""".stripMargin
    )

  @Test def `tuple` =
    checkEdit(
      """|object A{
         |  val (<<alpha>>, beta) = (123, 12)
         |}""".stripMargin,
      """|object A{
         |  val (alpha: Int, beta) = (123, 12)
         |}""".stripMargin
    )

  @Test def `tuple-inner` =
    checkEdit(
      """|object A{
         |  val ((<<alpha>>, gamma), beta) = ((123, 1), 12)
         |}""".stripMargin,
      """|object A{
         |  val ((alpha: Int, gamma), beta) = ((123, 1), 12)
         |}
         |""".stripMargin
    )

  @Test def `tuple-var` =
    checkEdit(
      """|object A{
         |  var (<<alpha>>, beta) = (123, 12)
         |}""".stripMargin,
      """|object A{
         |  var (alpha: Int, beta) = (123, 12)
         |}""".stripMargin
    )

  @Test def `var` =
    checkEdit(
      """|object A{
         |  var <<alpha>> = (123, 12)
         |}""".stripMargin,
      """|object A{
         |  var alpha: (Int, Int) = (123, 12)
         |}""".stripMargin
    )

  @Test def `def` =
    checkEdit(
      """|object A{
         |  def <<alpha>> = (123, 12)
         |}""".stripMargin,
      """|object A{
         |  def alpha: (Int, Int) = (123, 12)
         |}""".stripMargin
    )

  @Test def `def-comment` =
    checkEdit(
      """|object A{
         |  def <<alpha>> /* [] */= (123, 12)
         |}""".stripMargin,
      """|object A{
         |  def alpha: (Int, Int) /* [] */= (123, 12)
         |}
         |""".stripMargin
    )

  @Test def `def-comment-param` =
    checkEdit(
      """|object A{
         |  def <<alpha>>() /* [] */= (123, 12)
         |}""".stripMargin,
      """|object A{
         |  def alpha(): (Int, Int) /* [] */= (123, 12)
         |}
         |""".stripMargin
    )

  @Test def `def-param` =
    checkEdit(
      """|object A{
         |  def <<alpha>>(a : String) = (123, 12)
         |}""".stripMargin,
      """|object A{
         |  def alpha(a : String): (Int, Int) = (123, 12)
         |}""".stripMargin
    )

  @Test def `def-type-param` =
    checkEdit(
      """|object A{
         |  def <<alpha>>[T] = (123, 12)
         |}""".stripMargin,
      """|object A{
         |  def alpha[T]: (Int, Int) = (123, 12)
         |}""".stripMargin
    )

  @Test def `auto-import` =
    checkEdit(
      """|object A{
         |  val <<buffer>> = List("").toBuffer
         |}""".stripMargin,
      """|import scala.collection.mutable.Buffer
         |object A{
         |  val buffer: Buffer[String] = List("").toBuffer
         |}""".stripMargin
    )

  @Test def `lambda` =
    checkEdit(
      """|object A{
         |  val toStringList = List(1, 2, 3).map(<<int>> => int.toString)
         |}""".stripMargin,
      """|object A{
         |  val toStringList = List(1, 2, 3).map((int: Int) => int.toString)
         |}""".stripMargin
    )

  @Test def `lambda-existing-brace` =
    checkEdit(
      """|object A{
         |  val toStringList = List(1, 2, 3).map( /*{}*/(<<int>>) => int.toString)
         |}""".stripMargin,
      """|object A{
         |  val toStringList = List(1, 2, 3).map( /*{}*/(int: Int) => int.toString)
         |}""".stripMargin
    )

  @Test def `lambda-brace` =
    checkEdit(
      """|object A{
         |  val toStringList = List(1, 2, 3).map{<<int>> => int.toString}
         |}""".stripMargin,
      """|object A{
         |  val toStringList = List(1, 2, 3).map{(int: Int) => int.toString}
         |}""".stripMargin
    )

  @Test def `lambda-tuple` =
    checkEdit(
      """|object A{
         |  val toStringList = List((1, 2)).map((<<int>>, n) => int)
         |}""".stripMargin,
      """|object A{
         |  val toStringList = List((1, 2)).map((int: Int, n) => int)
         |}""".stripMargin
    )

  @Test def `pattern-match-paren` =
    checkEdit(
      """|object A{
         |  val list = List(1, 2, 3) match {
         |    case <<head>> :: tail => tail
         |    case Nil => Nil
         |  }
         |}""".stripMargin,
      """|object A{
         |  val list = List(1, 2, 3) match {
         |    case (head: Int) :: tail => tail
         |    case Nil => Nil
         |  }
         |}""".stripMargin
    )

  @Test def `pattern-match-tuple` =
    checkEdit(
      """|object A{
         |  val list = (1, 2) match {
         |    case (3, two) => 3
         |    case (one, <<two>>) => 2
         |  }
         |}""".stripMargin,
      """|object A{
         |  val list = (1, 2) match {
         |    case (3, two) => 3
         |    case (one, two: Int) => 2
         |  }
         |}
         |""".stripMargin
    )

  @Test def `pattern-match-option` =
    checkEdit(
      """|object A{
         |  Option(1) match {
         |    case Some(<<t>>) => t
         |    case None =>
         |  }
         |}""".stripMargin,
      """|object A{
         |  Option(1) match {
         |    case Some(t: Int) => t
         |    case None =>
         |  }
         |}
         |""".stripMargin
    )

  @Test def `pattern-match-list` =
    checkEdit(
      """|object A{
         |  List(1, 2, 3, 4) match {
         |    case List(<<t>>, next, other, _) => t
         |    case _ =>
         |  }
         |}""".stripMargin,
      """|object A{
         |  List(1, 2, 3, 4) match {
         |    case List(t: Int, next, other, _) => t
         |    case _ =>
         |  }
         |}
         |""".stripMargin
    )

  @Test def `pattern-match` =
    checkEdit(
      """|object A{
         |  val list = 1 match {
         |    case 2 => "Two!"
         |    case <<otherDigit>> => "Not two!"
         |  }
         |}""".stripMargin,
      """|object A{
         |  val list = 1 match {
         |    case 2 => "Two!"
         |    case otherDigit: 1 => "Not two!"
         |  }
         |}""".stripMargin
    )

  @Test def `for-comprehension` =
    checkEdit(
      """|object A{
         |  for {
         |    <<i>> <- 1 to 10
         |    j <- 1 to 11
         |  } yield (i, j)
         |}""".stripMargin,
      """|object A{
         |  for {
         |    i: Int <- 1 to 10
         |    j <- 1 to 11
         |  } yield (i, j)
         |}
         |""".stripMargin
    )

  @Test def `for-comprehension2` =
    checkEdit(
      """|object A{
         |  for {
         |    i <- 1 to 10
         |    <<j>> = i
         |  } yield (i, j)
         |}""".stripMargin,
      """|object A{
         |  for {
         |    i <- 1 to 10
         |    j: Int = i
         |  } yield (i, j)
         |}
         |""".stripMargin
    )

  @Test def `higher-kinded-types` =
    checkEdit(
      """|object Main2 {
         |
         |  trait Logger[T[_]]
         |  class Resource[F[_], A] {
         |    def map[B](f: A => B): Resource[F, B] = ???
         |  }
         |
         |  def mkLogger[F[_]]: Resource[F, Logger[F]] = ???
         |
         |  def <<serve>>[F[_]]() =
         |    for {
         |      logger <- mkLogger[F]
         |    } yield ()
         |
         |}
         |""".stripMargin,
      s"""|object Main2 {
          |
          |  trait Logger[T[_]]
          |  class Resource[F[_], A] {
          |    def map[B](f: A => B): Resource[F, B] = ???
          |  }
          |
          |  def mkLogger[F[_]]: Resource[F, Logger[F]] = ???
          |
          |  def serve[F[_]](): Resource[F, Unit] =
          |    for {
          |      logger <- mkLogger[F]
          |    } yield ()
          |
          |}
          |""".stripMargin
    )

  @Test def `path` =
    checkEdit(
      """|import java.nio.file.Paths
         |object ExplicitResultTypesPrefix {
         |  class Path
         |  def path = Paths.get("")
         |  object inner {
         |    val file = path
         |    object inner {
         |      val nio: java.nio.file.Path = path
         |      object inner {
         |        val <<java>> = path
         |      }
         |    }
         |  }
         |
         |}""".stripMargin,
      """|import java.nio.file.Paths
         |object ExplicitResultTypesPrefix {
         |  class Path
         |  def path = Paths.get("")
         |  object inner {
         |    val file = path
         |    object inner {
         |      val nio: java.nio.file.Path = path
         |      object inner {
         |        val java: _root_.java.nio.file.Path = path
         |      }
         |    }
         |  }
         |
         |}
         |""".stripMargin
    )

  @Test def `renamed` =
    checkEdit(
      """|import java.time.{Instant => I}
         |
         |trait Main {
         |  val every: I = ???
         |  val <<second>> = every
         |}
         |
         |""".stripMargin,
      """|import java.time.{Instant => I}
         |
         |trait Main {
         |  val every: I = ???
         |  val second: I = every
         |}
         |
         |""".stripMargin
    )

  @Test def `renamed-package` =
    checkEdit(
      """|import java.{ time => t }
         |
         |trait Main {
         |  val every: t.Instant = ???
         |  val <<second>> = every
         |}
         |
         |""".stripMargin,
      """|import java.{ time => t }
         |
         |trait Main {
         |  val every: t.Instant = ???
         |  val second: t.Instant = every
         |}
         |""".stripMargin
    )

  @Test def `renamed-package-long` =
    checkEdit(
      """|import scala.{ concurrent => c }
         |
         |trait Main {
         |  val every: c.duration.Duration = ???
         |  val <<second>> = every
         |}
         |
         |""".stripMargin,
      """|import scala.{ concurrent => c }
         |
         |trait Main {
         |  val every: c.duration.Duration = ???
         |  val second: c.duration.Duration = every
         |}
         |""".stripMargin
    )

  @Test def `error` =
    checkEdit(
      """|final case class Dependency(
         |    org: String,
         |    name: Option[String],
         |    version: Option[String]
         |)
         |
         |object Dependency {
         |  def <<apply>>(org: String) = Dependency(org, None, None)
         |  def apply(org: String, name: String) = Dependency(org, Some(name), None)
         |}
         |""".stripMargin,
      """|final case class Dependency(
         |    org: String,
         |    name: Option[String],
         |    version: Option[String]
         |)
         |
         |object Dependency {
         |  def apply(org: String): Any = Dependency(org, None, None)
         |  def apply(org: String, name: String) = Dependency(org, Some(name), None)
         |}
         |""".stripMargin
    )

  @Test def `either` =
    checkEdit(
      """|object O{
         |  def <<returnEither>>(value: String) = {
         |    if (value == "left") Left("a") else Right("b")
         |  }
         |}""".stripMargin,
      """|object O{
         |  def returnEither(value: String): Either[String, String] = {
         |    if (value == "left") Left("a") else Right("b")
         |  }
         |}
         |""".stripMargin
    )

  @Test def `backticks-1` =
    checkEdit(
      """|object O{
         |  val <<`bar`>> = 42
         |}""".stripMargin,
      """|object O{
         |  val `bar`: Int = 42
         |}
         |""".stripMargin
    )

  @Test def `backticks-2` =
    checkEdit(
      """|object O{
         |  def <<`bar`>> = 42
         |}""".stripMargin,
      """|object O{
         |  def `bar`: Int = 42
         |}
         |""".stripMargin
    )

  @Test def `backticks-3` =
    checkEdit(
      """|object O{
         |  List(1).map(<<`a`>> => a + 1)
         |}""".stripMargin,
      """|object O{
         |  List(1).map((`a`: Int) => a + 1)
         |}
         |""".stripMargin
    )

  @Test def `backticks-4` =
    checkEdit(
      """|case class `Foo-Foo`(i: Int)
       |object O{
       |  val <<foo>> = `Foo-Foo`(1)
       |}""".stripMargin,
      """|case class `Foo-Foo`(i: Int)
       |object O{
       |  val foo: `Foo-Foo` = `Foo-Foo`(1)
       |}
       |""".stripMargin
    )

  @Test def `backticks-5` =
    checkEdit(
      """|object A{
         |  case class `Foo-Foo`(i: Int)
         |}
         |object O{
         |  val <<foo>> = A.`Foo-Foo`(1)
         |}""".stripMargin,
      """|import A.`Foo-Foo`
         |object A{
         |  case class `Foo-Foo`(i: Int)
         |}
         |object O{
         |  val foo: `Foo-Foo` = A.`Foo-Foo`(1)
         |}
         |""".stripMargin
    )

  @Test def `backticks-6` =
    checkEdit(
      """|object A{
         |  case class `Foo-Foo`[A](i: A)
         |}
         |object O{
         |  val <<foo>> = A.`Foo-Foo`(1)
         |}""".stripMargin,
      """|import A.`Foo-Foo`
         |object A{
         |  case class `Foo-Foo`[A](i: A)
         |}
         |object O{
         |  val foo: `Foo-Foo`[Int] = A.`Foo-Foo`(1)
         |}
         |""".stripMargin
    )

  @Test def `backticks-7` =
    checkEdit(
      """|object A{
         |  class `x-x`
         |  case class Foo[A](i: A)
         |}
         |object O{
         |  val <<foo>> = A.Foo(new A.`x-x`)
         |}""".stripMargin,
      """|import A.`x-x`
         |object A{
         |  class `x-x`
         |  case class Foo[A](i: A)
         |}
         |object O{
         |  val foo: A.Foo[`x-x`] = A.Foo(new A.`x-x`)
         |}
         |""".stripMargin
    )

  @Test def `literal-types1` =
    checkEdit(
      """|object O {
         |  val a: Some[1] = Some(1)
         |  val <<b>> = a
         |}
         |""".stripMargin,
      """|object O {
         |  val a: Some[1] = Some(1)
         |  val b: Some[1] = a
         |}
         |""".stripMargin
    )

  @Test def `refined-types` =
    checkEdit(
      """|object O{
         |  trait Foo {
         |    type T
         |    type G
         |  }
         |
         |  val <<c>> = new Foo { type T = Int; type G = Long}
         |}
         |""".stripMargin,
      """|object O{
         |  trait Foo {
         |    type T
         |    type G
         |  }
         |
         |  val c: Foo{type T = Int; type G = Long} = new Foo { type T = Int; type G = Long}
         |}
         |""".stripMargin
    )

  @Test def `refined-types2` =
    checkEdit(
      """|object O{
         |  trait Foo {
         |    type T
         |  }
         |  val c = new Foo { type T = Int }
         |  val <<d>> = c
         |}
         |""".stripMargin,
      """|object O{
         |  trait Foo {
         |    type T
         |  }
         |  val c = new Foo { type T = Int }
         |  val d: Foo{type T = Int} = c
         |}
         |""".stripMargin
    )

  @Test def `refined-types3` =
    checkEdit(
      """|object O{
         |  trait Foo {
         |    type T
         |  }
         |
         |  val <<c>> = new Foo { type T = Int }
         |}
         |""".stripMargin,
      """|object O{
         |  trait Foo {
         |    type T
         |  }
         |
         |  val c: Foo{type T = Int} = new Foo { type T = Int }
         |}
         |""".stripMargin
    )

  @Test def `refined-types4` =
    checkEdit(
      """|trait Foo extends Selectable {
         |  type T
         |}
         |
         |val <<c>> = new Foo {
         |  type T = Int
         |  val x = 0
         |  def y = 0
         |  var z = 0
         |}
         |""".stripMargin,
      """|trait Foo extends Selectable {
         |  type T
         |}
         |
         |val c: Foo{type T = Int; val x: Int; def y: Int; val z: Int; def z_=(x$1: Int): Unit} = new Foo {
         |  type T = Int
         |  val x = 0
         |  def y = 0
         |  var z = 0
         |}
         |""".stripMargin
    )

  @Test def `dealias` =
    checkEdit(
      """|class Foo() {
         |  type T = Int
         |  def getT: T = 1
         |}
         |
         |object O {
         |  val <<c>> = new Foo().getT
         |}
         |""".stripMargin,
      """|class Foo() {
         |  type T = Int
         |  def getT: T = 1
         |}
         |
         |object O {
         |  val c: Int = new Foo().getT
         |}
         |""".stripMargin
    )

  @Test def `dealias2` =
    checkEdit(
      """|object Foo {
         |  type T = Int
         |  def getT: T = 1
         |  val <<c>> = getT
         |}
         |""".stripMargin,
      """|object Foo {
         |  type T = Int
         |  def getT: T = 1
         |  val c: T = getT
         |}
         |""".stripMargin
    )

  @Test def `dealias3` =
    checkEdit(
      """|object Foo:
         |  opaque type T = Int
         |  def getT: T = 1
         |val <<c>> = Foo.getT
         |""".stripMargin,
      """|import Foo.T
         |object Foo:
         |  opaque type T = Int
         |  def getT: T = 1
         |val c: T = Foo.getT
         |""".stripMargin
    )

  @Test def `dealias4` =
    checkEdit(
      """|object O:
         |  type M = Int
         |  type W = M => Int
         |  def get: W = ???
         |
         |val <<m>> = O.get
         |""".stripMargin,
      """|object O:
         |  type M = Int
         |  type W = M => Int
         |  def get: W = ???
         |
         |val m: Int => Int = O.get
         |""".stripMargin
    )

  @Test def `dealias5` =
    checkEdit(
      """|object O:
         |  opaque type M = Int
         |  type W = M => Int
         |  def get: W = ???
         |
         |val <<m>> = O.get
         |""".stripMargin,
      """|import O.M
         |object O:
         |  opaque type M = Int
         |  type W = M => Int
         |  def get: W = ???
         |
         |val m: M => Int = O.get
         |""".stripMargin
    )

  @Test def `import-rename` =
    checkEdit(
      """
        |package a
        |import scala.collection.{AbstractMap => AB}
        |
        |object Main {
        |  def test(): AB[Int, String] = ???
        |  val <<x>> = test()
        |}
        |""".stripMargin,
      """
        |package a
        |import scala.collection.{AbstractMap => AB}
        |
        |object Main {
        |  def test(): AB[Int, String] = ???
        |  val x: AB[Int, String] = test()
        |}
        |""".stripMargin
    )

  @Test def `operator-val` =
    checkEdit(
      """|object A {
         |  val <<!>> = 1
         |}
         |""".stripMargin,
      """|object A {
         |  val ! : Int = 1
         |}
         |""".stripMargin
    )

  @Test def `operator-def` =
    checkEdit(
      """|object A {
         |  def <<!>> = 1
         |}
         |""".stripMargin,
      """|object A {
         |  def ! : Int = 1
         |}
         |""".stripMargin
    )

  @Test def `operator-def-param` =
    checkEdit(
      """|object A {
         |  def <<!>>[T] = 1
         |}
         |""".stripMargin,
      """|object A {
         |  def ![T]: Int = 1
         |}
         |""".stripMargin
    )

  @Test def `operator-def-type-param` =
    checkEdit(
      """|object A {
         |  def <<!>>(x: Int) = 1
         |}
         |""".stripMargin,
      """|object A {
         |  def !(x: Int): Int = 1
         |}
         |""".stripMargin
    )

  @Test def `operator-for` =
    checkEdit(
      """|object A {
         |  def foo = for(<<!>> <- List(1)) yield !
         |}
         |""".stripMargin,
      """|object A {
         |  def foo = for(! : Int <- List(1)) yield !
         |}
         |""".stripMargin
    )
  @Test def `operator-lambda` =
    checkEdit(
      """|object A {
         |  val foo: Int => Int = (<<!>>) => ! + 1
         |}
         |""".stripMargin,
      """|object A {
         |  val foo: Int => Int = (! : Int) => ! + 1
         |}
         |""".stripMargin
    )

  @Test def `operator-ident` =
    checkEdit(
      """|object A {
         |  def foo =
         |    val ! = 1
         |    <<!>>
         |}
         |""".stripMargin,
      """|object A {
         |  def foo =
         |    val ! = 1
         |    ! : Int
         |}
         |""".stripMargin
    )

  @Test def `named-tuples` =
    checkEdit(
      """|def hello = (path = ".", num = 5)
         |
         |def <<test>> =
         |  hello ++ (line = 1)
         |
         |@main def bla =
         |   val x: (path: String, num: Int, line: Int) = test
         |""".stripMargin,
      """|def hello = (path = ".", num = 5)
         |
         |def test: (path : String, num : Int, line : Int) =
         |  hello ++ (line = 1)
         |
         |@main def bla =
         |   val x: (path: String, num: Int, line: Int) = test
         |""".stripMargin
    )

  @Test def `enums` =
    checkEdit(
      """|object EnumerationValue:
         |  object Day extends Enumeration {
         |    type Day = Value
         |    val Weekday, Weekend = Value
         |  }
         |  object Bool extends Enumeration {
         |    type Bool = Value
         |    val True, False = Value
         |  }
         |  import Bool._
         |  def day(d: Day.Value): Unit = ???
         |  val <<d>> =
         |    if (true) Day.Weekday
         |    else Day.Weekend
         |""".stripMargin,
      """|object EnumerationValue:
         |  object Day extends Enumeration {
         |    type Day = Value
         |    val Weekday, Weekend = Value
         |  }
         |  object Bool extends Enumeration {
         |    type Bool = Value
         |    val True, False = Value
         |  }
         |  import Bool._
         |  def day(d: Day.Value): Unit = ???
         |  val d: EnumerationValue.Day.Value =
         |    if (true) Day.Weekday
         |    else Day.Weekend
         |""".stripMargin
    )

  @Test def `enums2` =
    checkEdit(
      """|object EnumerationValue:
         |  object Day extends Enumeration {
         |    type Day = Value
         |    val Weekday, Weekend = Value
         |  }
         |  object Bool extends Enumeration {
         |    type Bool = Value
         |    val True, False = Value
         |  }
         |  import Bool._
         |  val <<b>> =
         |    if (true) True
         |    else False
         |""".stripMargin,
      """|object EnumerationValue:
         |  object Day extends Enumeration {
         |    type Day = Value
         |    val Weekday, Weekend = Value
         |  }
         |  object Bool extends Enumeration {
         |    type Bool = Value
         |    val True, False = Value
         |  }
         |  import Bool._
         |  val b: Value =
         |    if (true) True
         |    else False
         |""".stripMargin
    )

  @Test def `Adjust type for val` =
    checkEdit(
      """|object A{
         |  val <<alpha>>:String = 123
         |}""".stripMargin,
      """|object A{
         |  val alpha: Int = 123
         |}""".stripMargin
    )

  @Test def `Adjust type for val2` =
    checkEdit(
      """|object A{
         |  val <<alpha>>:Int = 123
         |}""".stripMargin,
      """|object A{
         |  val alpha: Int = 123
         |}""".stripMargin
    )

  @Test def `Adjust type for val3` =
    checkEdit(
      """|object A{
         |  val <<alpha>>: Int = 123
         |}""".stripMargin,
      """|object A{
         |  val alpha: Int = 123
         |}""".stripMargin
    )

  @Test def `Adjust type for def` =
    checkEdit(
      """|object A{
         |  def <<alpha>>:String = 123
         |}""".stripMargin,
      """|object A{
         |  def alpha: Int = 123
         |}""".stripMargin
    )

  @Test def `Adjust type for def2` =
    checkEdit(
      """|object A{
         |  def <<alpha>>:Int = 123
         |}""".stripMargin,
      """|object A{
         |  def alpha: Int = 123
         |}""".stripMargin
    )

  @Test def `Adjust type for def3` =
    checkEdit(
      """|object A{
         |  def <<alpha>>: Int = 123
         |}""".stripMargin,
      """|object A{
         |  def alpha: Int = 123
         |}""".stripMargin
    )

  def checkEdit(
      original: String,
      expected: String
  ): Unit =
    val edits = getAutoImplement(original)
    val (code, _, _) = params(original)
    val obtained = TextEdits.applyEdits(code, edits)
    assertNoDiff(expected, obtained)

  def getAutoImplement(
      original: String,
      filename: String = "file:/A.scala"
  ): List[l.TextEdit] =
    val (code, _, offset) = params(original)
    val result = presentationCompiler
      .insertInferredType(
        CompilerOffsetParams(URI.create(filename), code, offset, cancelToken)
      )
      .get()
    result.asScala.toList
