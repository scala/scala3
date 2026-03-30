package dotty.tools.pc.tests.definition

import scala.language.unsafeNulls
import scala.meta.internal.jdk.CollectionConverters.*
import scala.meta.pc.OffsetParams

import dotty.tools.pc.base.BasePcDefinitionSuite
import dotty.tools.pc.utils.MockEntries

import org.eclipse.lsp4j.Location
import org.junit.{Ignore, Test}

class PcDefinitionSuite extends BasePcDefinitionSuite:

  override def mockEntries = new MockEntries:
    override def definitions = Map[String, List[Location]](
      MockLocation("scala/Int#", "Int.scala"),
      MockLocation("scala/concurrent/Future#", "Future.scala"),
      MockLocation("scala/concurrent/Future.", "Future.scala"),
      MockLocation("scala/Option#withFilter().", "Option.scala"),
      MockLocation("scala/Option#flatMap().", "Option.scala"),
      MockLocation("scala/Option#map().", "Option.scala"),
      MockLocation("scala/Option#get().", "Option.scala"),
      MockLocation("scala/Predef.assert().", "Predef.scala"),
      MockLocation("scala/Predef.assert(+1).", "Predef.scala"),
      MockLocation("scala/Predef.Ensuring#ensuring().", "Predef.scala"),
      MockLocation("scala/Predef.Ensuring#ensuring(+1).", "Predef.scala"),
      MockLocation("scala/Predef.Ensuring#ensuring(+2).", "Predef.scala"),
      MockLocation("scala/Predef.Ensuring#ensuring(+3).", "Predef.scala"),
      MockLocation("scala/collection/immutable/List#`::`().", "List.scala"),
      MockLocation("scala/package.List.", "package.scala"),
      MockLocation("scala/collection/immutable/Vector.", "Vector.scala")
    )

  override def definitions(offsetParams: OffsetParams): List[Location] =
    presentationCompiler
      .definition(offsetParams)
      .get()
      .locations()
      .asScala
      .toList

  @Test def `basic` =
    check(
      """|
         |object Main {
         |  val <<abc>> = 42
         |  println(a@@bc)
         |}
         |""".stripMargin
    )

  @Test def `for` =
    check(
      """|object Main {
         |  for {
         |    <<x>> <- List(1)
         |    y <- 1.to(x)
         |    z = y + x
         |    if y < @@x
         |  } yield y
         |}
         |""".stripMargin
    )

  @Test def `for-flatMap` =
    check(
      """|
         |object Main {
         |  for {
         |    x /*scala/Option#flatMap(). Option.scala*/@@<- Option(1)
         |    y <- Option(x)
         |  } yield y
         |}
         |""".stripMargin
    )

  @Test def `for-map` =
    check(
      """|
         |object Main {
         |  for {
         |    x <- Option(1)
         |    y /*scala/Option#map(). Option.scala*/@@<- Option(x)
         |  } yield y
         |}
         |""".stripMargin
    )

  @Test def `for-withFilter` =
    check(
      """|
         |object Main {
         |  for {
         |    x <- Option(1)
         |    y <- Option(x)
         |    /*scala/Option#withFilter(). Option.scala*/@@if y > 2
         |  } yield y
         |}
         |""".stripMargin
    )

  @Test def `function` =
    check(
      """|
         |object Main {
         |  val <<increment>>: Int => Int = _ + 2
         |  incre@@ment(1)
         |}
         |""".stripMargin
    )

  // assert we don't go to `Tuple2.scala`
  @Test def `tuple` =
    check(
      """|
         |object Main {
         |  @@(1, 2)
         |}
         |""".stripMargin
    )

  @Test def `apply` =
    check(
      """|
         |object Main {
         |  /*scala/package.List. package.scala*/@@List(1)
         |}
         |""".stripMargin
    )

  @Test def `error` =
    check(
      """|
         |object Main {
         |  /*scala/Predef.assert(). Predef.scala*//*scala/Predef.assert(+1). Predef.scala*/@@assert
         |}
         |""".stripMargin
    )

  @Test def `error2` =
    check(
      """|
         |object Main {
         |  Predef./*scala/Predef.assert(). Predef.scala*//*scala/Predef.assert(+1). Predef.scala*/@@assert
         |}
         |""".stripMargin
    )

  @Test def `error3` =
    check(
      """|
         |object Main {
         |  1./*scala/Predef.Ensuring#ensuring(). Predef.scala*//*scala/Predef.Ensuring#ensuring(+1). Predef.scala*//*scala/Predef.Ensuring#ensuring(+2). Predef.scala*//*scala/Predef.Ensuring#ensuring(+3). Predef.scala*/@@ensuring
         |}
         |""".stripMargin
    )

  @Test def `new` =
    check(
      """|
         |object Main {
         |  ne@@w java.io.File("")
         |}
         |""".stripMargin
    )

  @Test def `extends` =
    check(
      """|
         |object Main ex@@tends java.io.Serializable {
         |}
         |""".stripMargin
    )

  @Test def `import1` =
    check(
      """|
         |import scala.concurrent./*scala/concurrent/Future# Future.scala*//*scala/concurrent/Future. Future.scala*/@@Future
         |object Main {
         |}
         |""".stripMargin
    )

  @Test def `import2` =
    check(
      """|
         |imp@@ort scala.concurrent.Future
         |object Main {
         |}
         |""".stripMargin
    )

  @Test def `import3` =
    check(
      """|
         |import scala.co@@ncurrent.Future
         |object Main {
         |}
         |""".stripMargin
    )

  @Test def exportType0 =
    check(
      """object Foo:
        |  trait <<Cat>>
        |object Bar:
        |  export Foo.*
        |class Test:
        |  import Bar.*
        |  def test = new Ca@@t {}
        |""".stripMargin
    )

  @Test def exportType1 =
    check(
      """object Foo:
        |  trait <<Cat>>[A]
        |object Bar:
        |  export Foo.*
        |class Test:
        |  import Bar.*
        |  def test = new Ca@@t[Int] {}
        |""".stripMargin
    )

  @Test def exportTerm0Nullary =
    check(
      """trait Foo:
        |  def <<meth>>: Int
        |class Bar(val foo: Foo):
        |  export foo.*
        |  def test(bar: Bar) = bar.me@@th
        |""".stripMargin
    )

  @Test def exportTerm0 =
    check(
      """trait Foo:
        |  def <<meth>>(): Int
        |class Bar(val foo: Foo):
        |  export foo.*
        |  def test(bar: Bar) = bar.me@@th()
        |""".stripMargin
    )

  @Test def exportTerm1 =
    check(
      """trait Foo:
        |  def <<meth>>(x: Int): Int
        |class Bar(val foo: Foo):
        |  export foo.*
        |  def test(bar: Bar) = bar.me@@th(0)
        |""".stripMargin
    )

  @Test def exportTerm1Poly =
    check(
      """trait Foo:
        |  def <<meth>>[A](x: A): A
        |class Bar(val foo: Foo):
        |  export foo.*
        |  def test(bar: Bar) = bar.me@@th(0)
        |""".stripMargin
    )

  @Test def exportTerm1Overload =
    check(
      """trait Foo:
        |  def <<meth>>(x: Int): Int
        |  def meth(x: String): String
        |class Bar(val foo: Foo):
        |  export foo.*
        |  def test(bar: Bar) = bar.me@@th(0)
        |""".stripMargin
    )

  @Test def exportTermExtension =
    check(
      """|package a
         |class Test extends A {
         |  assert("Hello".fo@@o == "HelloFoo")
         |}
         |
         |trait A {
         |  export B.*
         |}
         |
         |object B {
         |  extension (value: String) def <<foo>>: String = s"${value}Foo"
         |}
         |""".stripMargin
    )

  @Test def `named-arg-local` =
    check(
      """|
         |object Main {
         |  def foo(<<arg>>: Int): Unit = ()
         |
         |  foo(a@@rg = 42)
         |}
         |""".stripMargin
    )

  @Test def `named-arg-multiple` =
    check(
      """|object Main {
         |  def tst(par1: Int, par2: String, <<par3>>: Boolean): Unit = {}
         |
         |  tst(1, p@@ar3 = true, par2 = "")
         |}""".stripMargin
    )

  @Test def `named-arg-reversed` =
    check(
      """|object Main {
         |  def tst(par1: Int, <<par2>>: String): Unit = {}
         |
         |  tst(pa@@r2 = "foo", par1 = 1)
         |}
         |""".stripMargin
    )

  @Ignore // TODO SemanticdbSymbols.inverseSemanticdbSymbol does not support params and type params search
  @Test def `named-arg-global` =
    check(
      """|object Main {
         |  assert(/*scala/Predef.assert(+1).(assertion) Predef.scala*/@@assertion = true)
         |}
         |""".stripMargin
    )

  @Test def `symbolic-infix` =
    check(
      """|
         |object Main {
         |  val lst = 1 /*scala/collection/immutable/List#`::`(). List.scala*/@@:: Nil
         |}
         |""".stripMargin
    )

  @Test def `colon` =
    check(
      """|
         |object Main {
         |  val <<number>>@@: Int = 1
         |}
         |""".stripMargin
    )

  @Test def `package` =
    check(
      """|
         |object Main {
         |  val n = ma@@th.max(1, 2)
         |}
         |""".stripMargin
    )

  @Test def `eta` =
    check(
      """|
         |object Main {
         |  List(1).map(@@_ + 2)
         |}
         |""".stripMargin
    )

  @Test def `eta-2` =
    check(
      """|object Main {
         |  List(1).foldLeft(0)(_ + @@_)
         |}
         |""".stripMargin
    )

  @Test def `result-type` =
    check(
      """|
         |object Main {
         |  def x: /*scala/Int# Int.scala*/@@Int = 42
         |}
         |""".stripMargin
    )

  @Test def `constructor` =
    check(
      """|
         |class Main(x: /*scala/Int# Int.scala*/@@Int)
         |""".stripMargin
    )

  @Test def `case-class-apply` =
    check(
      """|
         |case class Foo(<<a>>: Int, b: String)
         |class Main {
         |  Foo(@@a = 3, b = "42")
         |}
         |""".stripMargin
    )

  @Test def `case-class-copy` =
    check(
      """|
         |case class Foo(<<a>>: Int, b: String)
         |class Main {
         |  Foo(2, "4").copy(@@a = 3, b = "42")
         |}
         |""".stripMargin
    )

  @Test def `do-not-point-at ::` =
    check(
      """|
         |class Main {
         |  val all = Option(42)./*scala/Option#get(). Option.scala*/@@get :: List("1", "2")
         |}
         |""".stripMargin
    )

  @Test def `synthetic-definition-case-class` =
    check(
      """|
         |class Main {
         |  case class <<User>>(name: String, age: Int)
         |  def hello(u: User): Unit = ()
         |  hello(Us@@er())
         |}
         |""".stripMargin
    )

  @Test def `synthetic-definition-class-constructor` =
    check(
      """|
         |class Main {
         |  class <<User>>(name: String, age: Int)
         |  def hello(u: User): Unit = ()
         |  hello(new Us@@er())
         |}
         |""".stripMargin
    )

  @Test def `no-definition-1` =
    check(
      """|
         |object Main {
         |  @@
         |  def foo() = {
         |    // this is a comment
         |  }
         |  println(foo())
         |}
         |""".stripMargin
    )

  @Test def `no-definition-2` =
    check(
      """|
         |object Main {
         |  def foo() = {
         |    @@// this is a comment
         |  }
         |  println(foo())
         |}
         |""".stripMargin
    )

  @Test def `no-definition-3` =
    check(
      """|
         |object Main {
         |  def foo() = {
         |    // th@@is is a comment
         |  }
         |  println(foo())
         |}
         |""".stripMargin
    )

  @Test def `enum-class-type-param` =
    check(
      """|
         |enum Options[<<AA>>]:
         |  case Some(x: A@@A)
         |  case None extends Options[Nothing]
         |""".stripMargin
    )

  @Test def `enum-class-type-param-covariant` =
    check(
      """|
         |enum Options[+<<AA>>]:
         |  case Some(x: A@@A)
         |  case None extends Options[Nothing]
         |""".stripMargin
    )

  @Test def `enum-class-type-param-duplicate` =
    check(
      """|
         |enum Testing[AA]:
         |  case Some[<<AA>>](x: A@@A) extends Testing[AA]
         |  case None extends Testing[Nothing]
         |""".stripMargin
    )

  @Test def `derives-def` =
    check(
      """|
         |import scala.deriving.Mirror
         |
         |trait <<Show>>[A]:
         |  def show(a: A): String
         |
         |object Show:
         |  inline def derived[T](using Mirror.Of[T]): Show[T] = new Show[T]:
         |    override def show(a: T): String = a.toString
         |
         |case class Box[A](value: A) derives Sh@@ow
         |
         |""".stripMargin
    )

  @Test def `implicit-extension` =
    check(
      """|class MyIntOut(val value: Int)
         |object MyIntOut:
         |  extension (i: MyIntOut) def <<uneven>> = i.value % 2 == 1
         |
         |val a = MyIntOut(1).un@@even
         |""".stripMargin
    )

  @Test def `named-tuples` =
    check(
      """|
         |val <<foo>> = (name = "Bob", age = 42, height = 1.9d)
         |val foo_name = foo.na@@me
         |""".stripMargin
    )

  @Test def `object` =
    check(
      """|package a
         |object <<Bar>> {
         |  def foo = 42
         |}
         |val m = B@@ar.foo
         |""".stripMargin
    )

  @Test def i7267 =
    check(
      """|package a
        |trait Foo {
        |  def someNum: Int
        |  def <<apply>>(i: Int): Unit = println(someNum)
        |}
        |object <<Bar>> extends Foo {
        |  def someNum = 42
        |}
        |
        |object Test {
        |  B@@ar(2)
        |}
        |""".stripMargin
    )

  @Test def `i7267-2` =
    check(
      """|package b
        |trait Foo {
        |  def someNum: Int
        |  def <<unapply>>(i: Int): Option[Int] = Some(i)
        |}
        |object <<Bar>> extends Foo {
        |  def someNum = 42
        |}
        |
        |object Test {
        |  Bar.someNum match {
        |    case B@@ar(1) => ???
        |    case _ =>
        |  }
        |}
        |""".stripMargin
    )

  @Test def `i7267-3` =
    check(
      """|package c
        |case class <<Bar>>()
        |object <<Bar>>
        |object O {
        |  val a = B@@ar()
        |}
        |""".stripMargin
    )

  @Test def `i7267-4` =
    check(
      """|package d
        |class <<Bar>>()
        |object <<Bar>> {
        |  def <<apply>>(): Bar = new Bar()
        |}
        |object O {
        |  val a = B@@ar()
        |}
        |""".stripMargin
    )

  @Test def i7256 =
    check(
      """|object Test:
         |  def <<methodA>>: Unit = ???
         |export Test.me@@thodA
         |""".stripMargin
    )

  @Test def `i7256-2` =
    check(
      """|object Test:
         |  def <<methodA>>: Unit = ???
         |  def methodB: Unit = ???
         |export Test.{me@@thodA, methodB}
         |""".stripMargin
    )

  @Test def `i7256-3` =
    check(
      """|object Test:
         |  def <<methodA>>: Unit = ???
         |  def methodB: Unit = ???
         |export Test.{methodA, methodB}
         |
         |val i = met@@hodA
         |""".stripMargin
    )

  @Test def i7427 =
    check(
      """|package a
         |object Repro:
         |    export scala.collection.immutable.V/*scala/collection/immutable/Vector. Vector.scala*/@@ector
         |""".stripMargin
    )

  @Test def i7763 =
    check(
      """|case class MyItem(<<name>>: String)
         |
         |def handle(item: MyItem) =
         |  item match {
         |    case MyItem(na@@me = n2) => println(n2)
         |  }
         |""".stripMargin
    )

  @Test def `i7763-neg` =
    check(
      """|object MyItem:
        |  def unapply(name: String): Option[Int] = ???
        |
        |def handle(item: String) =
        |  item match {
        |    case MyItem(na@@me = n2) => println(n2)
        |  }
        |""".stripMargin
    )

  @Test def `i7763-apply` =
    check(
      """|case class MyItem(<<name>>: String)
         |
         |def handle(item: String) = MyItem(na@@me = item)
         |""".stripMargin
    )
