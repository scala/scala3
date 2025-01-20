package dotty.tools.pc.tests.inlayHints

import dotty.tools.pc.base.BaseInlayHintsSuite

import org.junit.Test
class InlayHintsSuite extends BaseInlayHintsSuite {

  @Test def `local` =
    check(
     """|object Main {
        |  def foo() = {
        |    implicit val imp: Int = 2
        |    def addOne(x: Int)(implicit one: Int) = x + one
        |    val x = addOne(1)
        |  }
        |}
        |""".stripMargin,
     """|object Main {
        |  def foo()/*: Unit<<scala/Unit#>>*/ = {
        |    implicit val imp: Int = 2
        |    def addOne(x: Int)(implicit one: Int)/*: Int<<scala/Int#>>*/ = x + one
        |    val x/*: Int<<scala/Int#>>*/ = addOne(1)/*(using imp<<(3:17)>>)*/
        |  }
        |}
        |""".stripMargin
    )

  @Test def `type-params` =
    check(
     """|object Main {
        |  def hello[T](t: T) = t
        |  val x = hello(List(1))
        |}
        |""".stripMargin,
     """|object Main {
        |  def hello[T](t: T)/*: T<<(2:12)>>*/ = t
        |  val x/*: List<<scala/collection/immutable/List#>>[Int<<scala/Int#>>]*/ = hello/*[List<<scala/collection/immutable/List#>>[Int<<scala/Int#>>]]*/(List/*[Int<<scala/Int#>>]*/(1))
        |}
        |""".stripMargin
    )

  @Test def `type-params2` =
    check(
     """|object Main {
        |  def hello[T](t: T) = t
        |  val x = hello(Map((1,"abc")))
        |}
        |""".stripMargin,
     """|object Main {
        |  def hello[T](t: T)/*: T<<(2:12)>>*/ = t
        |  val x/*: Map<<scala/collection/immutable/Map#>>[Int<<scala/Int#>>, String<<java/lang/String#>>]*/ = hello/*[Map<<scala/collection/immutable/Map#>>[Int<<scala/Int#>>, String<<java/lang/String#>>]]*/(Map/*[Int<<scala/Int#>>, String<<java/lang/String#>>]*/((1,"abc")))
        |}
        |""".stripMargin,
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
        |  def addOne(x: Int)(implicit one: Int)/*: Int<<scala/Int#>>*/ = x + one
        |  val x/*: Int<<scala/Int#>>*/ = addOne(1)/*(using imp<<(3:15)>>)*/
        |}
        |""".stripMargin
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
        |  val y: User = /*intToUser<<(3:15)>>(*/1/*)*/
        |}
        |""".stripMargin
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
        |  def addOne(x: Int)(using one: Int)/*: Int<<scala/Int#>>*/ = x + one
        |  val x/*: Int<<scala/Int#>>*/ = addOne(1)/*(using imp<<(3:15)>>)*/
        |}
        |""".stripMargin
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
        |  val y: User = /*intToUser<<(3:8)>>(*/1/*)*/
        |}
        |""".stripMargin
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
        |  def doY/*: String<<scala/Predef.String#>>*/ = "7"
        |""".stripMargin
    )

  @Test def `basic` =
    check(
     """|object Main {
        |  val foo = 123
        |}
        |""".stripMargin,
     """|object Main {
        |  val foo/*: Int<<scala/Int#>>*/ = 123
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
        |  val foo/*: List<<scala/collection/immutable/List#>>[Int<<scala/Int#>>]*/ = List[Int](123)
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
        |  def m/*: List<<scala/collection/immutable/List#>>[Int<<scala/Int#>>]*/ = 1 :: List/*[Int<<scala/Int#>>]*/(1)
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
        |  val foo/*: Map<<scala/collection/immutable/Map#>>[Int<<scala/Int#>>, String<<java/lang/String#>>]*/ = Map/*[Int<<scala/Int#>>, String<<java/lang/String#>>]*/((1, "abc"))
        |}
        |""".stripMargin,
    )

  @Test def `tuple` =
    check(
     """|object Main {
        |  val foo = (123, 456)
        |}
        |""".stripMargin,
     """|object Main {
        |  val foo/*: (Int<<scala/Int#>>, Int<<scala/Int#>>)*/ = (123, 456)
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
        |  val foo/*: Buffer<<scala/collection/mutable/Buffer#>>[String<<java/lang/String#>>]*/ = List[String]("").toBuffer[String]
        |}
        |""".stripMargin,
    )

  @Test def `lambda-type` =
    check(
     """|object Main {
        |  val foo = () => 123
        |}
        |""".stripMargin,
     """|object Main {
        |  val foo/*: () => Int<<scala/Int#>>*/ = () => 123
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
        |  val foo/*: Int<<scala/Int#>>*/ = { val z/*: Int<<scala/Int#>>*/ = 123; z + 2}
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
        |  val c/*: Foo<<(2:8)>>{type T = Int<<scala/Int#>>; type G = Long<<scala/Long#>>}*/ = new Foo { type T = Int; type G = Long}
        |}
        |""".stripMargin
    )

  @Test def `refined-types2` =
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
        |  val c/*: Foo<<(2:8)>>{type T = Int<<scala/Int#>>}*/ = new Foo { type T = Int }
        |  val d/*: Foo<<(2:8)>>{type T = Int<<scala/Int#>>}*/ = c
        |}
        |""".stripMargin
    )

  @Test def `refined-types3` =
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
        |val c/*: Foo<<(1:6)>>{type T = Int<<scala/Int#>>; val x: Int<<scala/Int#>>; def y: Int<<scala/Int#>>; val z: Int<<scala/Int#>>; def z_=(x$1: Int<<scala/Int#>>): Unit<<scala/Unit#>>}*/ = new Foo {
        |  type T = Int
        |  val x/*: Int<<scala/Int#>>*/ = 0
        |  def y/*: Int<<scala/Int#>>*/ = 0
        |  var z/*: Int<<scala/Int#>>*/ = 0
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
        | val c/*: Int<<scala/Int#>>*/ = new Foo().getT
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
        |  val c/*: T<<(2:7)>>*/ = getT
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
        |val c/*: T<<(2:14)>>*/ = Foo.getT
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
        |val m/*: Int<<scala/Int#>> => Int<<scala/Int#>>*/ = O.get
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
        |val m/*: M<<(2:13)>> => Int<<scala/Int#>>*/ = O.get
        |""".stripMargin
    )

  @Test def `explicit-tuple` =
    check(
     """|object Main {
        |  val x = Tuple2.apply(1, 2)
        |}
        |""".stripMargin,
     """|object Main {
        |  val x/*: (Int<<scala/Int#>>, Int<<scala/Int#>>)*/ = Tuple2.apply/*[Int<<scala/Int#>>, Int<<scala/Int#>>]*/(1, 2)
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
        |  val x/*: (Int<<scala/Int#>>, Int<<scala/Int#>>)*/ = Tuple2/*[Int<<scala/Int#>>, Int<<scala/Int#>>]*/(1, 2)
        |}
        |""".stripMargin
    )

  @Test def `tuple-unapply` =
    check(
     """|object Main {
        |  val (local, _) = ("", 1.0)
        |  val (fst, snd) = (1, 2)
        |}
        |""".stripMargin,
     """|object Main {
        |  val (local/*: String<<java/lang/String#>>*/, _) = ("", 1.0)
        |  val (fst/*: Int<<scala/Int#>>*/, snd/*: Int<<scala/Int#>>*/) = (1, 2)
        |}
        |""".stripMargin,
     hintsInPatternMatch = true
    )

  @Test def `list-unapply` =
    check(
     """|object Main {
        |  val hd :: tail = List(1, 2)
        |}
        |""".stripMargin,
     """|object Main {
        |  val hd :: tail = List/*[Int<<scala/Int#>>]*/(1, 2)
        |}
        |""".stripMargin,
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
        |  val x/*: Int<<scala/Int#>>*/ = List/*[Int<<scala/Int#>>]*/(1, 2) match {
        |    case hd :: tail => hd
        |  }
        |}
        |""".stripMargin,
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
        |  val Foo(fst/*: Int<<scala/Int#>>*/, snd/*: Int<<scala/Int#>>*/) = Foo/*[Int<<scala/Int#>>]*/(1, 2)
        |}
        |""".stripMargin,
     hintsInPatternMatch = true
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
        |  val m/*: Int<<scala/Int#>>*/ = foo[500]/*(new ValueOf(...))*/
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

  @Test def `ord` =
    check(
     """|object Main {
        |  val ordered = "acb".sorted
        |}
        |""".stripMargin,
     """|object Main {
        |  val ordered/*: String<<scala/Predef.String#>>*/ = /*augmentString<<scala/Predef.augmentString().>>(*/"acb"/*)*/.sorted/*[Char<<scala/Char#>>]*//*(using Char<<scala/math/Ordering.Char.>>)*/
        |}
        |""".stripMargin
    )

  @Test def `partial-fun` =
    check(
     """|object Main {
        |  List(1).collect { case x => x }
        |  val x: PartialFunction[Int, Int] = {
        |    case 1 => 2
        |  }
        |}
        |""".stripMargin,
     """|object Main {
        |  List/*[Int<<scala/Int#>>]*/(1).collect/*[Int<<scala/Int#>>]*/ { case x => x }
        |  val x: PartialFunction[Int, Int] = {
        |    case 1 => 2
        |  }
        |}
        |""".stripMargin
    )

  @Test def `val-def-with-bind` =
    check(
     """|object O {
        |  val tupleBound @ (one, two) = ("1", "2")
        |}
        |""".stripMargin,
     """|object O {
        |  val tupleBound @ (one, two) = ("1", "2")
        |}
        |""".stripMargin
    )

  @Test def `val-def-with-bind-and-comment` =
    check(
     """|object O {
        |  val tupleBound /* comment */ @ (one, two) = ("1", "2")
        |}
        |""".stripMargin,
     """|object O {
        |  val tupleBound /* comment */ @ (one/*: String<<java/lang/String#>>*/, two/*: String<<java/lang/String#>>*/) = ("1", "2")
        |}
        |""".stripMargin,
     hintsInPatternMatch = true
    )

  @Test def `complex` =
    check(
     """|object ScalatestMock {
        |  class SRF
        |  implicit val subjectRegistrationFunction: SRF = new SRF()
        |  class Position
        |  implicit val here: Position = new Position()
        |  implicit class StringTestOps(name: String) {
        |    def should(right: => Unit)(implicit config: SRF): Unit = ()
        |    def in(f: => Unit)(implicit pos: Position): Unit = ()
        |  }
        |  implicit def instancesString: Eq[String] with Semigroup[String] = ???
        |}
        |
        |trait Eq[A]
        |trait Semigroup[A]
        |
        |class DemoSpec {
        |  import ScalatestMock._
        |
        |  "foo" should {
        |    "checkThing1" in {
        |      checkThing1[String]
        |    }
        |    "checkThing2" in {
        |      checkThing2[String]
        |    }
        |  }
        |
        |  "bar" should {
        |    "checkThing1" in {
        |      checkThing1[String]
        |    }
        |  }
        |
        |  def checkThing1[A](implicit ev: Eq[A]) = ???
        |  def checkThing2[A](implicit ev: Eq[A], sem: Semigroup[A]) = ???
        |}
        |""".stripMargin,
     """|object ScalatestMock {
        |  class SRF
        |  implicit val subjectRegistrationFunction: SRF = new SRF()
        |  class Position
        |  implicit val here: Position = new Position()
        |  implicit class StringTestOps(name: String) {
        |    def should(right: => Unit)(implicit config: SRF): Unit = ()
        |    def in(f: => Unit)(implicit pos: Position): Unit = ()
        |  }
        |  implicit def instancesString: Eq[String] with Semigroup[String] = ???
        |}
        |
        |trait Eq[A]
        |trait Semigroup[A]
        |
        |class DemoSpec {
        |  import ScalatestMock._
        |
        |  /*StringTestOps<<(6:17)>>(*/"foo"/*)*/ should {
        |    /*StringTestOps<<(6:17)>>(*/"checkThing1"/*)*/ in {
        |      checkThing1[String]/*(using instancesString<<(10:15)>>)*/
        |    }/*(using here<<(5:15)>>)*/
        |    /*StringTestOps<<(6:17)>>(*/"checkThing2"/*)*/ in {
        |      checkThing2[String]/*(using instancesString<<(10:15)>>, instancesString<<(10:15)>>)*/
        |    }/*(using here<<(5:15)>>)*/
        |  }/*(using subjectRegistrationFunction<<(3:15)>>)*/
        |
        |  /*StringTestOps<<(6:17)>>(*/"bar"/*)*/ should {
        |    /*StringTestOps<<(6:17)>>(*/"checkThing1"/*)*/ in {
        |      checkThing1[String]/*(using instancesString<<(10:15)>>)*/
        |    }/*(using here<<(5:15)>>)*/
        |  }/*(using subjectRegistrationFunction<<(3:15)>>)*/
        |
        |  def checkThing1[A](implicit ev: Eq[A])/*: Nothing<<scala/Nothing#>>*/ = ???
        |  def checkThing2[A](implicit ev: Eq[A], sem: Semigroup[A])/*: Nothing<<scala/Nothing#>>*/ = ???
        |}
        |""".stripMargin
    )

  @Test def `import-rename` =
    check(
     """|import scala.collection.{AbstractMap => AB}
        |import scala.collection.{Set => S}
        |
        |object Main {
        |  def test(d: S[Int], f: S[Char]): AB[Int, String] = {
        |    val x = d.map(_.toString)
        |    val y = f
        |    ???
        |  }
        |  val x = test(Set(1), Set('a'))
        |}
        |""".stripMargin,
     """|import scala.collection.{AbstractMap => AB}
        |import scala.collection.{Set => S}
        |
        |object Main {
        |  def test(d: S[Int], f: S[Char]): AB[Int, String] = {
        |    val x/*: S<<scala/collection/Set#>>[String<<java/lang/String#>>]*/ = d.map/*[String<<java/lang/String#>>]*/(_.toString)
        |    val y/*: S<<scala/collection/Set#>>[Char<<scala/Char#>>]*/ = f
        |    ???
        |  }
        |  val x/*: AB<<scala/collection/AbstractMap#>>[Int<<scala/Int#>>, String<<scala/Predef.String#>>]*/ = test(Set/*[Int<<scala/Int#>>]*/(1), Set/*[Char<<scala/Char#>>]*/('a'))
        |}
        |""".stripMargin,
    )

  @Test def `error-symbol` =
    check(
     """|package example
        |case class ErrorMessage(error)
        |""".stripMargin,
     """|package example
        |case class ErrorMessage(error)
        |""".stripMargin
    )

  @Test def `anonymous-given` =
    check(
     """|package example
        |
        |trait Ord[T]:
        |  def compare(x: T, y: T): Int
        |
        |given intOrd: Ord[Int] with
        |  def compare(x: Int, y: Int) =
        |    if x < y then -1 else if x > y then +1 else 0
        |
        |given Ord[String] with
        |  def compare(x: String, y: String) =
        |    x.compare(y)
        |
        |""".stripMargin,
     """|package example
        |
        |trait Ord[T]:
        |  def compare(x: T, y: T): Int
        |
        |given intOrd: Ord[Int] with
        |  def compare(x: Int, y: Int)/*: Int<<scala/Int#>>*/ =
        |    if x < y then -1 else if x > y then +1 else 0
        |
        |given Ord[String] with
        |  def compare(x: String, y: String)/*: Int<<scala/Int#>>*/ =
        |    /*augmentString<<scala/Predef.augmentString().>>(*/x/*)*/.compare(y)
        |
        |""".stripMargin
    )

  @Test def `context-bounds1` =
    check(
     """|package example
        |object O {
        |  given Int = 1
        |  def test[T: Ordering](x: T)(using Int) = ???
        |  test(1)
        |}
        |""".stripMargin,
     """|package example
        |object O {
        |  given Int = 1
        |  def test[T: Ordering](x: T)(using Int)/*: Nothing<<scala/Nothing#>>*/ = ???
        |  test/*[Int<<scala/Int#>>]*/(1)/*(using Int<<scala/math/Ordering.Int.>>, given_Int<<(2:8)>>)*/
        |}
        |""".stripMargin
    )

  @Test def `context-bounds2` =
    check(
     """|package example
        |object O {
        |  def test[T: Ordering](x: T) = ???
        |  test(1)
        |}
        |""".stripMargin,
     """|package example
        |object O {
        |  def test[T: Ordering](x: T)/*: Nothing<<scala/Nothing#>>*/ = ???
        |  test/*[Int<<scala/Int#>>]*/(1)/*(using Int<<scala/math/Ordering.Int.>>)*/
        |}
        |""".stripMargin
    )

  @Test def `context-bounds3` =
    check(
     """|package example
        |object O {
        |  def test[T: Ordering](x: T)(using Int) = ???
        |  test(1)
        |}
        |""".stripMargin,
     """|package example
        |object O {
        |  def test[T: Ordering](x: T)(using Int)/*: Nothing<<scala/Nothing#>>*/ = ???
        |  test/*[Int<<scala/Int#>>]*/(1)/*(using Int<<scala/math/Ordering.Int.>>)*/
        |}
        |""".stripMargin
    )

  @Test def `context-bounds4` =
    check(
     """|package example
        |object O {
        |  implicit val i: Int = 123
        |  def test[T: Ordering](x: T)(implicit v: Int) = ???
        |  test(1)
        |}
        |""".stripMargin,
     """|package example
        |object O {
        |  implicit val i: Int = 123
        |  def test[T: Ordering](x: T)(implicit v: Int)/*: Nothing<<scala/Nothing#>>*/ = ???
        |  test/*[Int<<scala/Int#>>]*/(1)/*(using Int<<scala/math/Ordering.Int.>>, i<<(2:15)>>)*/
        |}
        |""".stripMargin
    )

  @Test def `pattern-match` =
    check(
      """|package example
         |object O {
         |  val head :: tail = List(1)
         |  List(1) match {
         |    case head :: next =>
         |    case Nil =>
         |  }
         |  Option(Option(1)) match {
         |    case Some(Some(value)) =>
         |    case None =>
         |  }
         |  val (local, _) = ("", 1.0)
         |  val Some(x) = Option(1)
         |  for {
         |    x <- List((1,2))
         |    (z, y) = x
         |  } yield {
         |    x
         |  }
         |}
         |""".stripMargin,
      """|package example
         |object O {
         |  val head :: tail = List/*[Int<<scala/Int#>>]*/(1)
         |  List/*[Int<<scala/Int#>>]*/(1) match {
         |    case head :: next =>
         |    case Nil =>
         |  }
         |  Option/*[Option<<scala/Option#>>[Int<<scala/Int#>>]]*/(Option/*[Int<<scala/Int#>>]*/(1)) match {
         |    case Some(Some(value)) =>
         |    case None =>
         |  }
         |  val (local, _) = ("", 1.0)
         |  val Some(x) = Option/*[Int<<scala/Int#>>]*/(1)
         |  for {
         |    x <- List/*[(Int<<scala/Int#>>, Int<<scala/Int#>>)]*/((1,2))
         |    (z, y) = x
         |  } yield {
         |    x
         |  }
         |}
         |""".stripMargin
    )


  @Test def `pattern-match1` =
   check(
      """|package example
         |object O {
         |  val head :: tail = List(1)
         |  List(1) match {
         |    case head :: next =>
         |    case Nil =>
         |  }
         |  Option(Option(1)) match {
         |    case Some(Some(value)) =>
         |    case None =>
         |  }
         |  val (local, _) = ("", 1.0)
         |  val Some(x) = Option(1)
         |  for {
         |    x <- List((1,2))
         |    (z, y) = x
         |  } yield {
         |    x
         |  }
         |}
         |""".stripMargin,
      """|package example
         |object O {
         |  val head/*: Int<<scala/Int#>>*/ :: tail/*: List<<scala/collection/immutable/List#>>[Int<<scala/Int#>>]*/ = List/*[Int<<scala/Int#>>]*/(1)
         |  List/*[Int<<scala/Int#>>]*/(1) match {
         |    case head/*: Int<<scala/Int#>>*/ :: next/*: List<<scala/collection/immutable/List#>>[Int<<scala/Int#>>]*/ =>
         |    case Nil =>
         |  }
         |  Option/*[Option<<scala/Option#>>[Int<<scala/Int#>>]]*/(Option/*[Int<<scala/Int#>>]*/(1)) match {
         |    case Some(Some(value/*: Int<<scala/Int#>>*/)) =>
         |    case None =>
         |  }
         |  val (local/*: String<<java/lang/String#>>*/, _) = ("", 1.0)
         |  val Some(x/*: Int<<scala/Int#>>*/) = Option/*[Int<<scala/Int#>>]*/(1)
         |  for {
         |    x/*: (Int<<scala/Int#>>, Int<<scala/Int#>>)*/ <- List/*[(Int<<scala/Int#>>, Int<<scala/Int#>>)]*/((1,2))
         |    (z/*: Int<<scala/Int#>>*/, y/*: Int<<scala/Int#>>*/) = x
         |  } yield {
         |    x
         |  }
         |}
         |""".stripMargin,
      hintsInPatternMatch = true
   )

  @Test def quotes =
    check(
      """|package example
         |import scala.quoted.*
         |object O:
         |  inline def foo[T]: List[String] = ${fooImpl[T]}
         |  def fooImpl[T: Type](using Quotes): Expr[List[String]] = ???
         |""".stripMargin,
      """|package example
         |import scala.quoted.*
         |object O:
         |  inline def foo[T]: List[String] = ${fooImpl[T]}
         |  def fooImpl[T: Type](using Quotes): Expr[List[String]] = ???
         |""".stripMargin
    )

  @Test def quotes1 =
   check(
      """|package example
         |import scala.quoted.*
         |object O:
         |  def matchTypeImpl[T: Type](param1: Expr[T])(using Quotes) =
         |    import quotes.reflect.*
         |    Type.of[T] match
         |      case '[f] =>
         |        val fr = TypeRepr.of[T]
         |""".stripMargin,
      """|package example
         |import scala.quoted.*
         |object O:
         |  def matchTypeImpl[T: Type](param1: Expr[T])(using Quotes)/*: Unit<<scala/Unit#>>*/ =
         |    import quotes.reflect.*
         |    Type.of[T] match
         |      case '[f] =>
         |        val fr/*: TypeRepr<<scala/quoted/Quotes#reflectModule#TypeRepr#>>*/ = TypeRepr.of[T]/*(using evidence$1<<(3:27)>>)*/
         |""".stripMargin
   )


  @Test def quotes2 =
   check(
      """|package example
         |import scala.quoted.*
         |object O:
         |  def rec[A : Type](using Quotes): List[String] =
         |    Type.of[A] match
         |      case '[field *: fields] => ???
         |""".stripMargin,
      """|package example
         |import scala.quoted.*
         |object O:
         |  def rec[A : Type](using Quotes): List[String] =
         |    Type.of[A] match
         |      case '[field *: fields] => ???
         |""".stripMargin
   )

  @Test def `arg-apply` =
    check(
     """|object Main:
        |  case class A()
        |  case class B[T]()
        |  given A = A()
        |  implicit def bar(using a: A): B[A] = B[A]()
        |  def foo(using b: B[A]): String = "aaa"
        |  val g: String = foo
        |""".stripMargin,
     """|object Main:
        |  case class A()
        |  case class B[T]()
        |  given A = A()
        |  implicit def bar(using a: A): B[A] = B[A]()
        |  def foo(using b: B[A]): String = "aaa"
        |  val g: String = foo/*(using bar<<(5:15)>>)*/
        |""".stripMargin
    )
}
