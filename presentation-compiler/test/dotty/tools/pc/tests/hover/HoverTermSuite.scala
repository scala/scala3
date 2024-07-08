package dotty.tools.pc.tests.hover

import dotty.tools.pc.base.BaseHoverSuite

import org.junit.Test

class HoverTermSuite extends BaseHoverSuite:

  @Test def `map` =
    check(
      """object a {
        |  <<List(1).ma@@p(x => x.toString)>>
        |}
        |""".stripMargin,
      """|List[String]
         |override final def map[B](f: Int => B): List[B]""".stripMargin.hover
    )

  @Test def `app` =
    check(
      """|object Main extends <<Ap@@p>>{}
         |""".stripMargin,
      "trait App: App".hover
    )

  @Test def `apply` =
    check(
      """object a {
        |  <<Li@@st(1)>>.map(x => x.toString)
        |}
        |""".stripMargin,
      """|List[Int]
         |def apply[A](elems: A*): List[A]""".stripMargin.hover
    )

  @Test def `case-apply` =
    check(
      """case class Person(name: String)
        |object a {
        |  <<Per@@son("")>>
        |}
        |""".stripMargin,
      """|def apply(name: String): Person
         |""".stripMargin.hover
    )

  @Test def `interpolator-arg` =
    check(
      """
        |object a {
        |  val name = "John"
        |  <<s"Hello ${na@@me}">>
        |}
        |""".stripMargin,
      """|val name: String
         |""".stripMargin.hover
    )

  @Test def `interpolator-name` =
    check(
      """
        |object a {
        |  val name = "John"
        |  <<@@s"Hello ${name}">>
        |}
        |""".stripMargin,
      """|def s(args: Any*): String
         |""".stripMargin.hover
    )

  @Test def `interpolator-macro` =
    check(
      """
        |object a {
        |  val height = 1.9d
        |  val name = "James"
        |  <<@@f"$name%s is $height%2.2f meters tall">>
        |}
        |""".stripMargin,
      "def f[A >: Any](args: A*): String".hover
    )

  @Test def `interpolator-apply` =
    check(
      """
        |object a {
        |  implicit class Xtension(s: StringContext) {
        |    object num {
        |      def apply[T](a: T)(implicit ev: Int): T = ???
        |    }
        |  }
        |  implicit val n = 42
        |  <<@@num"Hello $n">>
        |}
        |""".stripMargin,
      """|Int
         |def apply[T](a: T)(implicit ev: Int): T
         |""".stripMargin.hover
    )

  @Test def `interpolator-unapply` =
    check(
      """
        |object a {
        |  implicit class Xtension(s: StringContext) {
        |    object num {
        |      def unapply(a: Int): Option[Int] = ???
        |    }
        |  }
        |  42 match {
        |    case nu@@m"$n" =>
        |  }
        |}
        |""".stripMargin,
      // https://github.com/lampepfl/dotty/issues/8835
      """|object num: a.Xtension
         |""".stripMargin.hover
    )

  @Test def `new` =
    check(
      """
        |class Foo(name: String, age: Int)
        |object a {
        |  <<new Fo@@o("", 42)>>
        |}
        |""".stripMargin,
      """|def this(name: String, age: Int): Foo
         |""".stripMargin.hover
    )

  @Test def `new-tparam` =
    check(
      """
        |class Foo[T](name: String, age: T)
        |object a {
        |  <<new Fo@@o("", 42)>>
        |}
        |""".stripMargin,
      """|Foo[Int]
         |def this[T](name: String, age: T): Foo[T]
         |""".stripMargin.hover
    )

  @Test def `new-tparam2` =
    check(
      """
        |class Foo[T](name: String, age: T)
        |object a {
        |  <<new Fo@@o[Int]("", 42)>>
        |}
        |""".stripMargin,
      "class Foo: Foo".hover
    )

  @Test def `new-anon` =
    check(
      """
        |class Foo(name: String, age: Int)
        |object a {
        |  new <<Fo@@o>>("", 42) {
        |    val x = 2
        |  }
        |}
        |""".stripMargin,
      "def this(name: String, age: Int): Foo".hover
    )

  @Test def `for-guard` =
    check(
      """
        |object a {
        |  for {
        |    x <- List(1)
        |    if <<@@x>> > 2
        |  } yield x
        |}
        |""".stripMargin,
      """|x: Int
         |""".stripMargin.hover
    )

  @Test def `for-flatMap` =
    check(
      """
        |object a {
        |  <<for {
        |    x <@@- Option(1)
        |    if x > 2
        |    y <- Some(x)
        |  } yield x.toString>>
        |}
        |""".stripMargin,
      """|Option[Int]#WithFilter
         |final def withFilter(p: A => Boolean): Option.this.WithFilter
         |""".stripMargin.hover
    )

  @Test def `for-map` =
    check(
      """
        |object a {
        |  for {
        |    x <- Option(1)
        |    if x > 2
        |    <<y <@@- Some(x)
        |  } yield x.toString>>
        |}
        |""".stripMargin,
      """|Option[String]
         |final def map[B](f: A => B): Option[B]
         |""".stripMargin.hover
    )

  @Test def `for-keyword` =
    check(
      """
        |object a {
        |  <<fo@@r {
        |    x <- Option(1)
        |    if x > 2
        |    y <- Some(x)
        |  } yield x.toString>>
        |}
        |""".stripMargin,
      """|Option[String]
         |def flatMap[B](f: Int => Option[B]): Option[B]
         |""".stripMargin.hover
    )

  @Test def `for-yield-keyword` =
    check(
      """
        |object a {
        |  for {
        |    x <- Option(1)
        |    if x > 2
        |    <<y <- Some(x.toLong)
        |  } yi@@eld x.toString>>
        |}
        |""".stripMargin,
      """|Option[String]
         |final def map[B](f: A => B): Option[B]
         |""".stripMargin.hover
    )

  @Test def `for-if-keyword` =
    check(
      """
        |object a {
        |  for {
        |    x <- <<Option(1)
        |    i@@f x > 2>>
        |    y <- Some(x)
        |  } yield x.toString
        |}
        |""".stripMargin,
      """|Option[Int]#WithFilter
         |final def withFilter(p: A => Boolean): Option.this.WithFilter
         """.stripMargin.hover
    )

  @Test def `for-method` =
    check(
      """
        |object a {
        |  for {
        |    x <- <<List(1).headOp@@tion>>
        |  } yield x
        |}
        |""".stripMargin,
      """|Option[Int]
         |override def headOption: Option[A]
         |""".stripMargin.hover
    )

  @Test def `object` =
    check(
      """
        |import java.nio.file._
        |object a {
        |  FileVisit@@Result.CONTINUE
        |}
        |""".stripMargin,
      """|enum FileVisitResult: java.nio.file
         |""".stripMargin.hover
    )

  @Test def `object2` =
    check(
      """package app
        |import java.nio.file._
        |object Outer {
        |  object Foo {
        |    class Inner
        |  }
        |}
        |object a {
        |  new Outer.Fo@@o.Inner
        |}
        |""".stripMargin,
      """|object Foo: app.Outer
         |""".stripMargin.hover
    )

  @Test def `import` =
    check(
      """
        |import java.n@@io.file._
        |""".stripMargin,
      """|```scala
         |package java.nio
         |```
         |""".stripMargin
    )

  @Test def `import2` =
    check(
      """
        |import jav@@a.nio.file._
        |""".stripMargin,
      """|```scala
         |package java
         |```
         |""".stripMargin
    )

  @Test def `import3` =
    check(
      """
        |import java.nio.fil@@e._
        |""".stripMargin,
      """|```scala
         |package java.nio.file
         |```
         |""".stripMargin
    )

  @Test def `import4` =
    check(
      """
        |import java.nio.file.{Fil@@es => File,Paths}
        |""".stripMargin,
      "object Files: java.nio.file".hover
    )

  @Test def `import5` =
    check(
      """
        |import java.nio.file.{Files => File,P@@aths}
        |""".stripMargin,
      "object Paths: java.nio.file".hover
    )

  @Test def `implicit-conv` =
    check(
      """
        |object Main {
        |  <<"".substring(0, 1).stripSu@@ffix("")>>
        |}
        |""".stripMargin,
      """|def stripSuffix(suffix: String): String
         |""".stripMargin.hover
    )

  @Test def `implicit-conv2` =
    check(
      """case class Text[T](value: T)
        |object Text {
        |  implicit def conv[T](value: T): Text[T] =
        |    Text(value)
        |}
        |object Main {
        |  def foo[T](text: Text[T]): T = text.value
        |  val number = 42
        |  foo(<<num@@ber>>)
        |}
        |""".stripMargin,
      """|val number: Int
         |""".stripMargin.hover
    )

  @Test def `widen` =
    check(
      """
        |object Main {
        |  println(<<java.nio.file.FileVisitResult.CONTIN@@UE>>)
        |}
        |""".stripMargin,
      "case CONTINUE: FileVisitResult".hover
    )

  @Test def `toplevel` =
    check(
      """|
         |val (first, <<se@@cond>>) = (1, false)
         |""".stripMargin,
      "val second: Boolean".hover
    )

  @Test def `annot` =
    check(
      """|import scala.annotation.tailrec
         |
         |object O {
         |  @<<tail@@rec>>
         |  def hello(n: Int): Int = {
         |    if (i == 0) 0
         |    else hello( n - 1)
         |  }
         |}
         |""".stripMargin,
      "def this(): tailrec".hover
    )

  @Test def `i5630` =
    check(
      """class MyIntOut(val value: Int)
        |object MyIntOut:
        |  extension (i: MyIntOut) def uneven = i.value % 2 == 1
        |
        |object Test:
        |  val a = MyIntOut(1).un@@even
        |""".stripMargin,
      """extension (i: MyIntOut) def uneven: Boolean
        |""".stripMargin.hover
    )

  @Test def `i5921` =
    check(
      """object Logarithms:
        |  trait Logarithm
        |  extension [K](vmap: Logarithm)
        |    def multiply(k: Logarithm): Logarithm = ???
        |
        |object Test:
        |  val in: Logarithms.Logarithm = ???
        |  in.multi@@ply(in)
        |""".stripMargin,
      "extension [K](vmap: Logarithm) def multiply(k: Logarithm): Logarithm".hover
    )

  @Test def `i5976` =
    check(
      """sealed trait ExtensionProvider {
        |  extension [A] (self: A) {
        |    def typeArg[B <: A]: B
        |    def noTypeArg: A
        |  }
        |}
        |
        |object Repro {
        |  def usage[A](f: ExtensionProvider ?=> A => Any): Any = ???
        |
        |  usage[Option[Int]](_.typeArg[Some[Int]].value.noTyp@@eArg.typeArg[Int])
        |}
        |""".stripMargin,
      """**Expression type**:
        |```scala
        |Int
        |```
        |**Symbol signature**:
        |```scala
        |extension [A](self: A) def noTypeArg: A
        |```
        |""".stripMargin
    )

  @Test def `i5976-1` =
    check(
      """sealed trait ExtensionProvider {
        |  extension [A] (self: A) {
        |    def typeArg[B <: A]: B
        |    def noTypeArg: A
        |  }
        |}
        |
        |object Repro {
        |  def usage[A](f: ExtensionProvider ?=> A => Any): Any = ???
        |
        |  usage[Option[Int]](_.type@@Arg[Some[Int]].value.noTypeArg.typeArg[Int])
        |}
        |""".stripMargin,
      """**Expression type**:
        |```scala
        |Some[Int]
        |```
        |**Symbol signature**:
        |```scala
        |extension [A](self: A) def typeArg[B <: A]: B
        |```
        |""".stripMargin
    )

  @Test def `i5977` =
    check(
      """sealed trait ExtensionProvider {
        |  extension [A] (self: A) {
        |    def typeArg[B <: A]: B
        |    def inferredTypeArg[C](value: C): C
        |  }
        |}
        |
        |object Repro {
        |  def usage[A](f: ExtensionProvider ?=> A => Any): Any = ???
        |
        |  usage[Option[Int]](_.infer@@redTypeArg("str"))
        |}
        |""".stripMargin,
      """**Expression type**:
        |```scala
        |String
        |```
        |**Symbol signature**:
        |```scala
        |extension [A](self: A) def inferredTypeArg[C](value: C): C
        |```
        |""".stripMargin
    )

  @Test def `i5977-1` =
    check(
      """sealed trait ExtensionProvider {
        |  extension [A] (self: A) {
        |    def typeArg[B <: A]: B
        |    def inferredTypeArg[C](value: C): C
        |  }
        |}
        |
        |object Repro {
        |  def usage[A](f: ExtensionProvider ?=> A => Any): Any = ???
        |
        |  usage[Option[Int]](_.infer@@redTypeArg[String]("str"))
        |}
        |""".stripMargin,
      """**Expression type**:
        |```scala
        |String
        |```
        |**Symbol signature**:
        |```scala
        |extension [A](self: A) def inferredTypeArg[C](value: C): C
        |```
        |""".stripMargin
    )

  @Test def `i5977-2` =
    check(
      """sealed trait ExtensionProvider {
        |  extension [A] (self: A) {
        |    def typeArg[B <: A]: B
        |    def inferredTypeArg[C](value: C): C
        |  }
        |}
        |
        |object Repro {
        |  def usage[A](f: ExtensionProvider ?=> A => Any): Any = ???
        |
        |  usage[Option[Int]](_.typeArg[Some[Int]].value.infer@@redTypeArg("str"))
        |}
        |""".stripMargin,
      """**Expression type**:
        |```scala
        |String
        |```
        |**Symbol signature**:
        |```scala
        |extension [A](self: A) def inferredTypeArg[C](value: C): C
        |```
        |""".stripMargin
    )

  @Test def `i5977-3` =
    check(
      """sealed trait ExtensionProvider {
        |  extension [A] (self: A) {
        |    def typeArg[B <: A]: B
        |    def inferredTypeArg[C](value: C): C
        |  }
        |}
        |
        |object Repro {
        |  def usage[A](f: ExtensionProvider ?=> A => Any): Any = ???
        |
        |  usage[Option[Int]](_.typeArg[Some[Int]].value.infer@@redTypeArg[String]("str"))
        |}
        |""".stripMargin,
      """**Expression type**:
        |```scala
        |String
        |```
        |**Symbol signature**:
        |```scala
        |extension [A](self: A) def inferredTypeArg[C](value: C): C
        |```
        |""".stripMargin
    )

  @Test def `import-rename` =
    check(
      """
        |import scala.collection.{AbstractMap => AB}
        |import scala.collection.{Set => S}
        |
        |object Main {
        |  def test(): AB[Int, String] = ???
        |  <<val t@@t = test()>>
        |}
        |""".stripMargin,
      """
        |```scala
        |type AB = AbstractMap
        |```
        |
        |```scala
        |val tt: AB[Int, String]
        |```""".stripMargin,
    )

  @Test def `import-rename2` =
    check(
      """
        |import scala.collection.{AbstractMap => AB}
        |import scala.collection.{Set => S}
        |
        |object Main {
        |  <<def te@@st(d: S[Int], f: S[Char]): AB[Int, String] = ???>>
        |}
        |""".stripMargin,
      """
        |```scala
        |type S = Set
        |type AB = AbstractMap
        |```
        |
        |```scala
        |def test(d: S[Int], f: S[Char]): AB[Int, String]
        |```""".stripMargin,
  )

  @Test def `import-no-rename` =
    check(
      """
        |import scala.collection
        |
        |object O {
        |  <<val ab@@c = collection.Map(1 -> 2)>>
        |}
        |""".stripMargin,
      """
        |```scala
        |val abc: scala.collection.Map[Int, Int]
        |```
        |""".stripMargin
    )

  @Test def `dealias-type-members-in-structural-types1`: Unit =
    check(
      """object Obj {
        |  trait A extends Sup { self =>
        |    type T
        |    def member : T
        |  }
        |  val x: A { type T = Int} = ???
        |
        |  <<x.mem@@ber>>
        |
        |}""".stripMargin,
      """def member: Int""".stripMargin.hover
  )

  @Test def `dealias-type-members-in-structural-types2`: Unit =
    check(
      """object Obj:
        |  trait A extends Sup { self =>
        |    type T
        |    def fun(body: A { type T = self.T} => Unit) = ()
        |  }
        |  val x: A { type T = Int} = ???
        |
        |  x.fun: <<y@@y>> =>
        |    ()
        |""".stripMargin,
      """yy: A{type T = Int}""".stripMargin.hover
  )

  @Test def `right-assoc-extension`: Unit =
    check(
      """
        |case class Wrap[+T](x: T)
        |
        |extension [T](a: T)
        |  def <<*@@:>>[U <: Tuple](b: Wrap[U]): Wrap[T *: U] = Wrap(a *: b.x)
        |""".stripMargin,
      "extension [T](a: T) def *:[U <: Tuple](b: Wrap[U]): Wrap[T *: U]".hover
    )
