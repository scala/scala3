package dotty.tools.pc.tests.decorations

import scala.meta.internal.pc.DecorationKind

import dotty.tools.pc.base.BaseSyntheticDecorationsSuite

import org.junit.Test

class SyntheticDecorationsSuite extends BaseSyntheticDecorationsSuite:

  @Test def `type-params` = 
    check(
      """|object Main {
         |  def hello[T](t: T) = t
         |  val x = hello(List(1))
         |}
         |""".stripMargin,
      """|object Main {
         |  def hello[T](t: T) = t
         |  val x = hello[List[Int]](List[Int](1))
         |}
         |""".stripMargin,
      kind = Some(DecorationKind.TypeParameter)
    )

  @Test def `type-params2` = 
    check(
      """|object Main {
         |  def hello[T](t: T) = t
         |  val x = hello(Map((1,"abc")))
         |}
         |""".stripMargin,
      """|object Main {
         |  def hello[T](t: T) = t
         |  val x = hello[Map[Int, String]](Map[Int, String]((1,"abc")))
         |}
         |""".stripMargin,
      kind = Some(DecorationKind.TypeParameter)
    )

  @Test def `implicit-param` = 
    check(
      """|case class User(name: String)
         |object Main {
         |  implicit val imp: Int = 2
         |  def addOne(x: Int)(implicit one: Int) = x + one
         |  val x = addOne(1)
         |}
         |""".stripMargin,
      """|case class User(name: String)
         |object Main {
         |  implicit val imp: Int = 2
         |  def addOne(x: Int)(implicit one: Int) = x + one
         |  val x = addOne(1)(imp)
         |}
         |""".stripMargin,
      kind = Some(DecorationKind.ImplicitParameter)
    )

  @Test def `implicit-conversion` = 
    check(
      """|case class User(name: String)
         |object Main {
         |  implicit def intToUser(x: Int): User = new User(x.toString)
         |  val y: User = 1
         |}
         |""".stripMargin,
      """|case class User(name: String)
         |object Main {
         |  implicit def intToUser(x: Int): User = new User(x.toString)
         |  val y: User = intToUser(1)
         |}
         |""".stripMargin,
      kind = Some(DecorationKind.ImplicitConversion)
    )

  @Test def `using-param` = 
    check(
      """|case class User(name: String)
         |object Main {
         |  implicit val imp: Int = 2
         |  def addOne(x: Int)(using one: Int) = x + one
         |  val x = addOne(1)
         |}
         |""".stripMargin,
      """|case class User(name: String)
         |object Main {
         |  implicit val imp: Int = 2
         |  def addOne(x: Int)(using one: Int) = x + one
         |  val x = addOne(1)(imp)
         |}
         |""".stripMargin,
      kind = Some(DecorationKind.ImplicitParameter)
    )

  @Test def `given-conversion` = 
    check(
      """|case class User(name: String)
         |object Main {
         |  given intToUser: Conversion[Int, User] = User(_.toString)
         |  val y: User = 1
         |}
         |""".stripMargin,
      """|case class User(name: String)
         |object Main {
         |  given intToUser: Conversion[Int, User] = User(_.toString)
         |  val y: User = intToUser(1)
         |}
         |""".stripMargin,
      kind = Some(DecorationKind.ImplicitConversion)
    )

  @Test def `given-conversion2` = 
    check(
      """|trait Xg:
         |  def doX: Int
         |trait Yg:
         |  def doY: String
         |given (using Xg): Yg with
         |  def doY = "7"
         |""".stripMargin,
      """|trait Xg:
         |  def doX: Int
         |trait Yg:
         |  def doY: String
         |given (using Xg): Yg with
         |  def doY: String = "7"
         |""".stripMargin
    )

  @Test def `basic` = 
    check(
      """|object Main {
         |  val foo = 123
         |}
         |""".stripMargin,
      """|object Main {
         |  val foo: Int = 123
         |}
         |""".stripMargin
    )

  @Test def `list` = 
    check(
      """|object Main {
         |  val foo = List[Int](123)
         |}
         |""".stripMargin,
      """|object Main {
         |  val foo: List[Int] = List[Int](123)
         |}
         |""".stripMargin
    )

  @Test def `list2` = 
    check(
      """|object O {
         |  def m = 1 :: List(1)
         |}
         |""".stripMargin,
      """|object O {
         |  def m: List[Int] = 1 ::[Int] List[Int](1)
         |}
         |""".stripMargin
    )

  @Test def `two-param` = 
    check(
      """|object Main {
         |  val foo = Map((1, "abc"))
         |}
         |""".stripMargin,
      """|object Main {
         |  val foo: Map[Int, String] = Map[Int, String]((1, "abc"))
         |}
         |""".stripMargin
    )

  @Test def `tuple` = 
    check(
      """|object Main {
         |  val foo = (123, 456)
         |}
         |""".stripMargin,
      """|object Main {
         |  val foo: (Int, Int) = (123, 456)
         |}
         |""".stripMargin
    )

  @Test def `import-needed` = 
    check(
      """|object Main {
         |  val foo = List[String]("").toBuffer[String]
         |}
         |""".stripMargin,
      """|object Main {
         |  val foo: Buffer[String] = List[String]("").toBuffer[String]
         |}
         |""".stripMargin
    )

  @Test def `lambda-type` = 
    check(
    """|object Main {
       |  val foo = () => 123
       |}
       |""".stripMargin,
    """|object Main {
       |  val foo: () => Int = () => 123
       |}
       |""".stripMargin
    )

  @Test def `block` = 
    check(
      """|object Main {
         |  val foo = { val z = 123; z + 2}
         |}
         |""".stripMargin,
      """|object Main {
         |  val foo: Int = { val z: Int = 123; z + 2}
         |}
         |""".stripMargin
    )

  @Test def `refined-types` = 
    check(
      """|object O{
         |  trait Foo {
         |    type T
         |    type G
         |  }
         |
         |  val c = new Foo { type T = Int; type G = Long}
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

  @Test def `refined-types1` = 
    check(
      """|object O{
         |  trait Foo {
         |    type T
         |  }
         |  val c = new Foo { type T = Int }
         |  val d = c
         |}
         |""".stripMargin,
      """|object O{
         |  trait Foo {
         |    type T
         |  }
         |  val c: Foo{type T = Int} = new Foo { type T = Int }
         |  val d: Foo{type T = Int} = c
         |}
         |""".stripMargin
    )

  @Test def `refined-types4` = 
    check(
      """|trait Foo extends Selectable {
         |  type T
         |}
         |
         |val c = new Foo {
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
         |  val x: Int = 0
         |  def y: Int = 0
         |  var z: Int = 0
         |}
         |""".stripMargin
    )

  @Test def `dealias` = 
    check(
      """|class Foo() {
         |  type T = Int
         |  def getT: T = 1
         |}
         |
         |object O {
         | val c = new Foo().getT
         |}
         |""".stripMargin,
      """|class Foo() {
         |  type T = Int
         |  def getT: T = 1
         |}
         |
         |object O {
         | val c: Int = new Foo().getT
         |}
         |""".stripMargin
    )

  @Test def `dealias2` = 
    check(
      """|object Foo {
         |  type T = Int
         |  def getT: T = 1
         |  val c = getT
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
    check(
      """|object Foo:
         |  opaque type T = Int
         |  def getT: T = 1
         |val c = Foo.getT
         |""".stripMargin,
      """|object Foo:
         |  opaque type T = Int
         |  def getT: T = 1
         |val c: T = Foo.getT
         |""".stripMargin
    )

  @Test def `dealias4` = 
    check(
      """|object O:
         | type M = Int
         | type W = M => Int
         | def get: W = ???
         |
         |val m = O.get
         |""".stripMargin,
      """|object O:
         | type M = Int
         | type W = M => Int
         | def get: W = ???
         |
         |val m: Int => Int = O.get
         |""".stripMargin
    )

  @Test def `dealias5` = 
    check(
      """|object O:
         | opaque type M = Int
         | type W = M => Int
         | def get: W = ???
         |
         |val m = O.get
         |""".stripMargin,
      """|object O:
         | opaque type M = Int
         | type W = M => Int
         | def get: W = ???
         |
         |val m: M => Int = O.get
         |""".stripMargin
    )

  @Test def `explicit-tuple` = 
    check(
      """|object Main {
         |  val x = Tuple2.apply(1, 2)
         |}
         |""".stripMargin,
      """|object Main {
         |  val x: (Int, Int) = Tuple2.apply[Int, Int](1, 2)
         |}
         |""".stripMargin
    )

  @Test def `explicit-tuple1` = 
    check(
      """|object Main {
         |  val x = Tuple2(1, 2)
         |}
         |""".stripMargin,
      """|object Main {
         |  val x: (Int, Int) = Tuple2[Int, Int](1, 2)
         |}
         |""".stripMargin
    )

  @Test def `tuple-unapply` = 
    check(
      """|object Main {
         |  val (fst, snd) = (1, 2)
         |}
         |""".stripMargin,
      """|object Main {
         |  val (fst: Int, snd: Int) = (1, 2)
         |}
         |""".stripMargin
    )

  @Test def `list-unapply` = 
    check(
      """|object Main {
         |  val hd :: tail = List(1, 2)
         |}
         |""".stripMargin,
      """|object Main {
         |  val hd: Int ::[Int] tail: List[Int] = List[Int](1, 2)
         |}
         |""".stripMargin
    )

  @Test def `list-match` = 
    check(
      """|object Main {
         |  val x = List(1, 2) match {
         |    case hd :: tail => hd
         |  }
         |}
         |""".stripMargin,
      """|object Main {
         |  val x: Int = List[Int](1, 2) match {
         |    case hd: Int ::[Int] tail: List[Int] => hd
         |  }
         |}
         |""".stripMargin
    )

  @Test def `case-class-unapply` = 
    check(
      """|object Main {
         |case class Foo[A](x: A, y: A)
         |  val Foo(fst, snd) = Foo(1, 2)
         |}
         |""".stripMargin,
      """|object Main {
         |case class Foo[A](x: A, y: A)
         |  val Foo[Int](fst: Int, snd: Int) = Foo[Int](1, 2)
         |}
         |""".stripMargin
    )

  @Test def `valueOf` = 
    check(
      """|object O {
         |  def foo[Total <: Int](implicit total: ValueOf[Total]): Int = total.value
         |  val m = foo[500]
         |}
         |""".stripMargin,
      """|object O {
         |  def foo[Total <: Int](implicit total: ValueOf[Total]): Int = total.value
         |  val m: Int = foo[500](new ValueOf(...))
         |}
         |""".stripMargin
    )

  @Test def `case-class1` = 
    check(
      """|object O {
         |case class A(x: Int, g: Int)(implicit y: String)
         |}
         |""".stripMargin,
      """|object O {
         |case class A(x: Int, g: Int)(implicit y: String)
         |}
         |""".stripMargin
    )

