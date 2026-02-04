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
        |    val x/*: Int<<scala/Int#>>*/ = addOne(/*x = */1)/*(using imp<<(3:17)>>)*/
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
        |  val x/*: List<<scala/collection/immutable/List#>>[Int<<scala/Int#>>]*/ = hello/*[List<<scala/collection/immutable/List#>>[Int<<scala/Int#>>]]*/(/*t = */List/*[Int<<scala/Int#>>]*/(/*elems = */1))
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
        |  val x/*: Map<<scala/collection/immutable/Map#>>[Int<<scala/Int#>>, String<<java/lang/String#>>]*/ = hello/*[Map<<scala/collection/immutable/Map#>>[Int<<scala/Int#>>, String<<java/lang/String#>>]]*/(/*t = */Map/*[Int<<scala/Int#>>, String<<java/lang/String#>>]*/(/*elems = */(1,"abc")))
        |}
        |""".stripMargin
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
        |  val x/*: Int<<scala/Int#>>*/ = addOne(/*x = */1)/*(using imp<<(3:15)>>)*/
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
        |  implicit def intToUser(x: Int): User = new User(/*name = */x.toString)
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
        |  val x/*: Int<<scala/Int#>>*/ = addOne(/*x = */1)/*(using imp<<(3:15)>>)*/
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
        |  given intToUser: Conversion[Int, User] = User(/*name = */_.toString)
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
        |  val foo/*: List<<scala/collection/immutable/List#>>[Int<<scala/Int#>>]*/ = List[Int](/*elems = */123)
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
        |  def m/*: List<<scala/collection/immutable/List#>>[Int<<scala/Int#>>]*/ = 1 :: List/*[Int<<scala/Int#>>]*/(/*elems = */1)
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
        |  val foo/*: Map<<scala/collection/immutable/Map#>>[Int<<scala/Int#>>, String<<java/lang/String#>>]*/ = Map/*[Int<<scala/Int#>>, String<<java/lang/String#>>]*/(/*elems = */(1, "abc"))
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
        |  val foo/*: Buffer<<scala/collection/mutable/Buffer#>>[String<<java/lang/String#>>]*/ = List[String](/*elems = */"").toBuffer[String]
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
        |  val x/*: (Int<<scala/Int#>>, Int<<scala/Int#>>)*/ = Tuple2.apply/*[Int<<scala/Int#>>, Int<<scala/Int#>>]*/(/*_1 = */1, /*_2 = */2)
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
        |  val x/*: (Int<<scala/Int#>>, Int<<scala/Int#>>)*/ = Tuple2/*[Int<<scala/Int#>>, Int<<scala/Int#>>]*/(/*_1 = */1, /*_2 = */2)
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
        |  val hd :: tail = List/*[Int<<scala/Int#>>]*/(/*elems = */1, 2)
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
        |  val x/*: Int<<scala/Int#>>*/ = List/*[Int<<scala/Int#>>]*/(/*elems = */1, 2) match {
        |    case hd :: tail => hd
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
        |  val Foo(fst/*: Int<<scala/Int#>>*/, snd/*: Int<<scala/Int#>>*/) = Foo/*[Int<<scala/Int#>>]*/(/*x = */1, /*y = */2)
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
        |  List/*[Int<<scala/Int#>>]*/(/*elems = */1).collect/*[Int<<scala/Int#>>]*/ { case x => x }
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
        |  /*StringTestOps<<(6:17)>>(*/"foo"/*)*/ should {/*=> */
        |    /*StringTestOps<<(6:17)>>(*/"checkThing1"/*)*/ in {/*=> */
        |      checkThing1[String]/*(using instancesString<<(10:15)>>)*/
        |    }/*(using here<<(5:15)>>)*/
        |    /*StringTestOps<<(6:17)>>(*/"checkThing2"/*)*/ in {/*=> */
        |      checkThing2[String]/*(using instancesString<<(10:15)>>, instancesString<<(10:15)>>)*/
        |    }/*(using here<<(5:15)>>)*/
        |  }/*(using subjectRegistrationFunction<<(3:15)>>)*/
        |
        |  /*StringTestOps<<(6:17)>>(*/"bar"/*)*/ should {/*=> */
        |    /*StringTestOps<<(6:17)>>(*/"checkThing1"/*)*/ in {/*=> */
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
        |    val x/*: S<<scala/collection/Set#>>[String<<java/lang/String#>>]*/ = d.map/*[String<<java/lang/String#>>]*/(/*f = */_.toString)
        |    val y/*: S<<scala/collection/Set#>>[Char<<scala/Char#>>]*/ = f
        |    ???
        |  }
        |  val x/*: AB<<scala/collection/AbstractMap#>>[Int<<scala/Int#>>, String<<java/lang/String#>>]*/ = test(/*d = */Set/*[Int<<scala/Int#>>]*/(/*elems = */1), /*f = */Set/*[Char<<scala/Char#>>]*/(/*elems = */'a'))
        |}
        |""".stripMargin
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
        |    /*augmentString<<scala/Predef.augmentString().>>(*/x/*)*/.compare(/*that = */y)
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
        |  test/*[Int<<scala/Int#>>]*/(/*x = */1)/*(using Int<<scala/math/Ordering.Int.>>, given_Int<<(2:8)>>)*/
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
        |  test/*[Int<<scala/Int#>>]*/(/*x = */1)/*(using Int<<scala/math/Ordering.Int.>>)*/
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
        |  test/*[Int<<scala/Int#>>]*/(/*x = */1)/*(using Int<<scala/math/Ordering.Int.>>)*/
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
        |  test/*[Int<<scala/Int#>>]*/(/*x = */1)/*(using Int<<scala/math/Ordering.Int.>>, i<<(2:15)>>)*/
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
         |  val head :: tail = List/*[Int<<scala/Int#>>]*/(/*elems = */1)
         |  List/*[Int<<scala/Int#>>]*/(/*elems = */1) match {
         |    case head :: next =>
         |    case Nil =>
         |  }
         |  Option/*[Option<<scala/Option#>>[Int<<scala/Int#>>]]*/(/*x = */Option/*[Int<<scala/Int#>>]*/(/*x = */1)) match {
         |    case Some(Some(value)) =>
         |    case None =>
         |  }
         |  val (local, _) = ("", 1.0)
         |  val Some(x) = Option/*[Int<<scala/Int#>>]*/(/*x = */1)
         |  for {
         |    x <- List/*[(Int<<scala/Int#>>, Int<<scala/Int#>>)]*/(/*elems = */(1,2))
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
         |  val head/*: Int<<scala/Int#>>*/ :: tail/*: List<<scala/collection/immutable/List#>>[Int<<scala/Int#>>]*/ = List/*[Int<<scala/Int#>>]*/(/*elems = */1)
         |  List/*[Int<<scala/Int#>>]*/(/*elems = */1) match {
         |    case head/*: Int<<scala/Int#>>*/ :: next/*: List<<scala/collection/immutable/List#>>[Int<<scala/Int#>>]*/ =>
         |    case Nil =>
         |  }
         |  Option/*[Option<<scala/Option#>>[Int<<scala/Int#>>]]*/(/*x = */Option/*[Int<<scala/Int#>>]*/(/*x = */1)) match {
         |    case Some(Some(value/*: Int<<scala/Int#>>*/)) =>
         |    case None =>
         |  }
         |  val (local/*: String<<java/lang/String#>>*/, _) = ("", 1.0)
         |  val Some(x/*: Int<<scala/Int#>>*/) = Option/*[Int<<scala/Int#>>]*/(/*x = */1)
         |  for {
         |    x/*: (Int<<scala/Int#>>, Int<<scala/Int#>>)*/ <- List/*[(Int<<scala/Int#>>, Int<<scala/Int#>>)]*/(/*elems = */(1,2))
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
        |  val g: String = foo/*(using bar<<(5:15)>>(given_A<<(4:8)>>))*/
        |""".stripMargin
    )

  @Test def `multiple-params-list` =
    check(
      """|object Main {
         |  case class A()
         |  case class B()
         |  implicit val theA: A = A()
         |  def foo(b: B)(implicit a: A): String = "aaa"
         |  val g: String = foo(B())
         |}
         |""".stripMargin,
      """|object Main {
         |  case class A()
         |  case class B()
         |  implicit val theA: A = A()
         |  def foo(b: B)(implicit a: A): String = "aaa"
         |  val g: String = foo(/*b = */B())/*(using theA<<(4:15)>>)*/
         |}
         |""".stripMargin
    )

  @Test def `implicit-chain` =
    check(
      """|object Main{
         |  def hello()(implicit string: String, integer: Int, long: Long): String = {
         |    println(s"Hello $string, $long, $integer!")
         |  }
         |  implicit def theString(implicit i: Int): String = i.toString
         |  implicit def theInt(implicit l: Long): Int = l
         |  implicit val theLong: Long = 42
         |  hello()
         |}
         |""".stripMargin,
      """|object Main{
         |  def hello()(implicit string: String, integer: Int, long: Long): String = {
         |    println(/*x = */s"Hello $string, $long, $integer!")
         |  }
         |  implicit def theString(implicit i: Int): String = i.toString
         |  implicit def theInt(implicit l: Long): Int = l
         |  implicit val theLong: Long = 42
         |  hello()/*(using theString<<(5:15)>>(theInt<<(6:15)>>(theLong<<(7:15)>>)), theInt<<(6:15)>>(theLong<<(7:15)>>), theLong<<(7:15)>>)*/
         |}
         |""".stripMargin
    )

  @Test def `implicit-parameterless-def` =
    check(
      """|object Main{
         |  def hello()(implicit string: String, integer: Int, long: Long): String = {
         |    println(s"Hello $string, $long, $integer!")
         |  }
         |  implicit def theString(implicit i: Int): String = i.toString
         |  implicit def theInt: Int = 43
         |  implicit def theLong: Long = 42
         |  hello()
         |}
         |""".stripMargin,
      """|object Main{
         |  def hello()(implicit string: String, integer: Int, long: Long): String = {
         |    println(/*x = */s"Hello $string, $long, $integer!")
         |  }
         |  implicit def theString(implicit i: Int): String = i.toString
         |  implicit def theInt: Int = 43
         |  implicit def theLong: Long = 42
         |  hello()/*(using theString<<(5:15)>>(theInt<<(6:15)>>), theInt<<(6:15)>>, theLong<<(7:15)>>)*/
         |}
         |""".stripMargin
    )

  @Test def `implicit-fn` =
    check(
      """|object Main{
        |  implicit def stringLength(s: String): Int = s.length
        |  implicitly[String => Int]
        |
        |  implicit val namedStringLength: String => Long = (s: String) => s.length.toLong
        |  implicitly[String => Long]
        |}
        |""".stripMargin,
      """|object Main{
        |  implicit def stringLength(s: String): Int = s.length
        |  implicitly[String => Int]
        |
        |  implicit val namedStringLength: String => Long = (s: String) => s.length.toLong
        |  implicitly[String => Long]/*(using namedStringLength<<(5:15)>>)*/
        |}
        |""".stripMargin
    )

  @Test def `implicit-fn2` =
    check(
      """|object Main{
         |  implicit def stringLength(s: String, i: Int): Int = s.length
         |  implicitly[(String, Int) => Int]
         |}
         |""".stripMargin,
      """|object Main{
         |  implicit def stringLength(s: String, i: Int): Int = s.length
         |  implicitly[(String, Int) => Int]
         |}
         |""".stripMargin
    )

  @Test def `strip-margin` =
    check(
      """|object Main{
         |  "".stripMargin
         |}
         |""".stripMargin,
      """|package test
         |object Main{
         |  /*augmentString<<scala/Predef.augmentString().>>(*/""/*)*/.stripMargin
         |}
         |""".stripMargin
    )

  @Test def `named-tuples` =
    check(
      """|def hello = (path = ".", num = 5)
         |
         |def test =
         |  hello ++ (line = 1)
         |
         |@main def bla =
         |   val x: (path: String, num: Int, line: Int) = test
         |""".stripMargin,
      """|def hello/*: (path : String<<java/lang/String#>>, num : Int<<scala/Int#>>)*/ = (path = ".", num = 5)/*[(String<<java/lang/String#>>, Int<<scala/Int#>>)]*/
         |
         |def test/*: (path : String<<java/lang/String#>>, num : Int<<scala/Int#>>, line : Int<<scala/Int#>>)*/ =
         |  hello ++/*[Tuple1<<scala/Tuple1#>>["line"], Tuple1<<scala/Tuple1#>>[Int<<scala/Int#>>]]*/ (line = 1)/*[Tuple1<<scala/Tuple1#>>[Int<<scala/Int#>>]]*//*(using refl<<scala/`<:<`.refl().>>)*/
         |
         |@main def bla/*: Unit<<scala/Unit#>>*/ =
         |   val x: (path: String, num: Int, line: Int) = test
         |""".stripMargin
    )

  @Test def `named-tuple-false-negative` =
    check(
      """|def hello(test: Int) = (path = ".", num = test)
         |
         |@main def test2 =
         |   val x = hello(7)
         |""".stripMargin,
      """|def hello(test: Int)/*: (path : String<<java/lang/String#>>, num : Int<<scala/Int#>>)*/ = (path = ".", num = test)/*[(String<<java/lang/String#>>, Int<<scala/Int#>>)]*/
         |
         |@main def test2/*: Unit<<scala/Unit#>>*/ =
         |   val x/*: (path : String<<java/lang/String#>>, num : Int<<scala/Int#>>)*/ = hello(/*test = */7)
         |""".stripMargin
    )

  @Test def `by-name-regular` =
    check(
      """|object Main:
         |  def foo(x: => Int, y: Int, z: => Int)(w: Int, v: => Int): Unit = ()
         |  foo(1, 2, 3)(4, 5)
         |""".stripMargin,
      """|object Main:
         |  def foo(x: => Int, y: Int, z: => Int)(w: Int, v: => Int): Unit = ()
         |  foo(/*x = => */1, /*y = */2, /*z = => */3)(/*w = */4, /*v = => */5)
         |""".stripMargin
    )

  @Test def `by-name-block` =
    check(
      """|object Main:
         |  def Future[A](arg: => A): A = arg
         |
         |  Future(1 + 2)
         |  Future {
         |    1 + 2
         |  }
         |  Future {
         |    val x = 1
         |    val y = 2
         |    x + y
         |  }
         |  Some(Option(2)
         |    .getOrElse {
         |      List(1,2)
         |        .headOption
         |    })
         |""".stripMargin,
      """|object Main:
         |  def Future[A](arg: => A): A = arg
         |
         |  Future/*[Int<<scala/Int#>>]*/(/*arg = => */1 + 2)
         |  Future/*[Int<<scala/Int#>>]*/ {/*=> */
         |    1 + 2
         |  }
         |  Future/*[Int<<scala/Int#>>]*/ {/*=> */
         |    val x/*: Int<<scala/Int#>>*/ = 1
         |    val y/*: Int<<scala/Int#>>*/ = 2
         |    x + y
         |  }
         |  Some/*[Int<<scala/Int#>> | Option<<scala/Option#>>[Int<<scala/Int#>>]]*/(/*value = */Option/*[Int<<scala/Int#>>]*/(/*x = */2)
         |    .getOrElse/*[Int<<scala/Int#>> | Option<<scala/Option#>>[Int<<scala/Int#>>]]*/ {/*=> */
         |      List/*[Int<<scala/Int#>>]*/(/*elems = */1,2)
         |        .headOption
         |    })
         |""".stripMargin
    )

  @Test def `by-name-for-comprehension` =
    check(
      """|object Main:
         |  case class Test[A](v: A):
         |     def flatMap(f: => (A => Test[Int])): Test[Int] = f(v)
         |     def map(f: => (A => Int)): Test[Int] = Test(f(v))
         |
         |  def main(args: Array[String]): Unit =
         |    val result: Test[Int] = for {
         |      a <- Test(10)
         |      b <- Test(20)
         |    } yield a + b
         |
         |""".stripMargin,
      """|object Main:
         |  case class Test[A](v: A):
         |     def flatMap(f: => (A => Test[Int])): Test[Int] = f(/*v1 = */v)
         |     def map(f: => (A => Int)): Test[Int] = Test/*[Int<<scala/Int#>>]*/(/*v = */f(/*v1 = */v))
         |
         |  def main(args: Array[String]): Unit =
         |    val result: Test[Int] = for {
         |      a <- Test/*[Int<<scala/Int#>>]*/(/*v = */10)
         |      b <- Test/*[Int<<scala/Int#>>]*/(/*v = */20)
         |    } yield a + b
         |
         |""".stripMargin
    )

  @Test def `by-name-for-comprehension-generic` =
    check(
      """|object Main:
         |  case class Test[A](v: A):
         |     def flatMap[B](f: => (A => Test[B])): Test[B] = f(v)
         |     def map[B](f: => (A => B)): Test[B] = Test(f(v))
         |
         |  def main(args: Array[String]): Unit =
         |    val result: Test[Int] = for {
         |      a <- Test(10)
         |      b <- Test(20)
         |    } yield a + b
         |
         |""".stripMargin,
      """|object Main:
         |  case class Test[A](v: A):
         |     def flatMap[B](f: => (A => Test[B])): Test[B] = f(/*v1 = */v)
         |     def map[B](f: => (A => B)): Test[B] = Test/*[B<<(4:13)>>]*/(/*v = */f(/*v1 = */v))
         |
         |  def main(args: Array[String]): Unit =
         |    val result: Test[Int] = for {
         |      a <- Test/*[Int<<scala/Int#>>]*/(/*v = */10)
         |      b <- Test/*[Int<<scala/Int#>>]*/(/*v = */20)
         |    } yield a + b
         |
         |""".stripMargin
    )

  @Test def `by-name-method-infix-extension` =
    check(
      """|case class A[T, U](dummy: Int, name: U):
         |  def compute: Int = 1
         |
         |object A:
         |  extension [T, U](a: A[T, U])
         |     def ++(other: => A[T, U]): Int = a.dummy + other.dummy + a.compute
         |
         |object Main:
         |  val a = A[Int, String](0, "foo")
         |  val res = a ++ a
         |""".stripMargin,
      """|case class A[T, U](dummy: Int, name: U):
         |  def compute: Int = 1
         |
         |object A:
         |  extension [T, U](a: A[T, U])
         |     def ++(other: => A[T, U]): Int = a.dummy + other.dummy + a.compute
         |
         |object Main:
         |  val a/*: A<<(1:11)>>[Int<<scala/Int#>>, String<<scala/Predef.String#>>]*/ = A[Int, String](/*dummy = */0, /*name = */"foo")
         |  val res/*: Int<<scala/Int#>>*/ = a ++/*[Int<<scala/Int#>>, String<<java/lang/String#>>]*/ a
         |""".stripMargin
    )

  @Test def `by-name-method-infix-extension-2` =
    check(
      """|case class A[T, U](dummy: Int, name: U):
         |  def compute: Int = 1
         |
         |extension [T, U](a: A[T, U])
         |  def ++(other: => A[T, U]): Int = a.dummy + other.dummy + a.compute
         |
         |object Main:
         |  val a = A[Int, String](0, "foo")
         |  val res = a ++ a
         |""".stripMargin,
      """|case class A[T, U](dummy: Int, name: U):
         |  def compute: Int = 1
         |
         |extension [T, U](a: A[T, U])
         |  def ++(other: => A[T, U]): Int = a.dummy + other.dummy + a.compute
         |
         |object Main:
         |  val a/*: A<<(1:11)>>[Int<<scala/Int#>>, String<<scala/Predef.String#>>]*/ = A[Int, String](/*dummy = */0, /*name = */"foo")
         |  val res/*: Int<<scala/Int#>>*/ = a ++/*[Int<<scala/Int#>>, String<<java/lang/String#>>]*/ a
         |""".stripMargin
    )

  @Test def `named-parameter` =
    check(
      """|object Main{
         |  def hello[T](arg: T) = arg
         |  val x = hello(arg = List(1))
         |}
         |""".stripMargin,
      """|object Main{
         |  def hello[T](arg: T)/*: T<<(2:12)>>*/ = arg
         |  val x/*: List<<scala/collection/immutable/List#>>[Int<<scala/Int#>>]*/ = hello/*[List<<scala/collection/immutable/List#>>[Int<<scala/Int#>>]]*/(arg = List/*[Int<<scala/Int#>>]*/(/*elems = */1))
         |}
         |""".stripMargin
    )

  @Test def `java-method-call` =
    check(
      """|object Main {
        |  val str = "hello"
        |  val sub = str.substring(1, 3)
        |  val replaced = str.replace('l', 'x')
        |}
        |""".stripMargin,
      """|object Main {
        |  val str/*: String<<java/lang/String#>>*/ = "hello"
        |  val sub/*: String<<java/lang/String#>>*/ = str.substring(1, 3)
        |  val replaced/*: String<<java/lang/String#>>*/ = str.replace('l', 'x')
        |}
        |""".stripMargin
    )

  @Test def `default-parameter` =
    check(
      """|object Main {
         |  def foo(a: Int, b: Int = 2) = a + b
         |  val x = foo(1)
         |}
         |""".stripMargin,
      """|object Main {
         |  def foo(a: Int, b: Int = 2)/*: Int<<scala/Int#>>*/ = a + b
         |  val x/*: Int<<scala/Int#>>*/ = foo(/*a = */1)
         |}
         |""".stripMargin
    )

  @Test def `default-parameter-2` =
    check(
      """|object Main {
         |  def foo(a: Int = 10, b: Int = 2) = a + b
         |  val x = foo(b = 1)
         |}
         |""".stripMargin,
      """|object Main {
         |  def foo(a: Int = 10, b: Int = 2)/*: Int<<scala/Int#>>*/ = a + b
         |  val x/*: Int<<scala/Int#>>*/ = foo(b = 1)
         |}
         |""".stripMargin
    )

  @Test def `default-parameter-3` =
    check(
      """|object Main {
         |  def foo(a: Int, b: Int = 2, c: Int) = a + b + c
         |  val x = foo(a = 1, c = 2)
         |}
         |""".stripMargin,
      """|object Main {
         |  def foo(a: Int, b: Int = 2, c: Int)/*: Int<<scala/Int#>>*/ = a + b + c
         |  val x/*: Int<<scala/Int#>>*/ = foo(a = 1, c = 2)
         |}
         |""".stripMargin
    )

  @Test def `default-parameter-4` =
    check(
      """|object Main {
         |  def foo(a: Int, b: Int = 2, c: Int) = a + b + c
         |  val x = foo(1, 2, 3)
         |}
         |""".stripMargin,
      """|object Main {
         |  def foo(a: Int, b: Int = 2, c: Int)/*: Int<<scala/Int#>>*/ = a + b + c
         |  val x/*: Int<<scala/Int#>>*/ = foo(/*a = */1, /*b = */2, /*c = */3)
         |}
         |""".stripMargin
    )

  @Test def `xray-single-chain-same-line` =
    check(
      """|object Main{
         |  trait Bar {
         |   def bar: Bar
         |  }
         |
         |  trait Foo {
         |    def foo(): Foo
         |  }
         |
         |val bar: Bar = ???
         |val foo: Foo = ???
         |
         |val thing1: Bar = bar.bar
         |val thing2: Foo = foo.foo()
         |}
         |""".stripMargin,
      """|object Main{
         |  trait Bar {
         |   def bar: Bar
         |  }
         |
         |  trait Foo {
         |    def foo(): Foo
         |  }
         |
         |val bar: Bar = ???
         |val foo: Foo = ???
         |
         |val thing1: Bar = bar.bar
         |val thing2: Foo = foo.foo()
         |}
         |""".stripMargin
    )

  @Test def `xray-multi-chain-same-line` =
    check(
      """|object Main{
         |  trait Bar {
         |   def bar: Bar
         |  }
         |
         |  trait Foo {
         |    def foo(): Foo
         |  }
         |
         |val bar: Bar = ???
         |val foo: Foo = ???
         |
         |val thing1: Bar = bar.bar.bar
         |val thing2: Foo = foo.foo().foo()
         |}
         |""".stripMargin,
      """|object Main{
         |  trait Bar {
         |   def bar: Bar
         |  }
         |
         |  trait Foo {
         |    def foo(): Foo
         |  }
         |
         |val bar: Bar = ???
         |val foo: Foo = ???
         |
         |val thing1: Bar = bar.bar.bar
         |val thing2: Foo = foo.foo().foo()
         |}
         |""".stripMargin
    )

  @Test def `xray-single-chain-new-line` =
    check(
      """|object Main{
         |  trait Bar {
         |   def bar: Bar
         |  }
         |
         |  trait Foo {
         |    def foo(): Foo
         |  }
         |
         |val bar: Bar = ???
         |val foo: Foo = ???
         |
         |val thing1: Bar = bar
         |  .bar
         |val thing2: Foo = foo
         |  .foo()
         |}
         |""".stripMargin,
      """|object Main{
         |  trait Bar {
         |   def bar: Bar
         |  }
         |
         |  trait Foo {
         |    def foo(): Foo
         |  }
         |
         |val bar: Bar = ???
         |val foo: Foo = ???
         |
         |val thing1: Bar = bar
         |  .bar
         |val thing2: Foo = foo
         |  .foo()
         |}
         |""".stripMargin
    )

  @Test def `xray-simple-chain` =
    check(
      """|object Main{
         |  trait Foo {
         |   def bar: Bar
         |  }
         |
         |  trait Bar {
         |    def foo(): Foo
         |  }
         |
         |val foo: Foo = ???
         |
         |val thingy: Bar = foo
         |  .bar
         |  .foo()
         |  .bar
         |}
         |""".stripMargin,
      """|object Main{
         |  trait Foo {
         |   def bar: Bar
         |  }
         |
         |  trait Bar {
         |    def foo(): Foo
         |  }
         |
         |val foo: Foo = ???
         |
         |val thingy: Bar = foo
         |  .bar/*  : Bar<<(6:8)>>*/
         |  .foo()/*: Foo<<(2:8)>>*/
         |  .bar/*  : Bar<<(6:8)>>*/
         |}
         |""".stripMargin
    )

  @Test def `xray-long-chain` =
    check(
      """|object Main{
         |  trait Foo[F] {
         |   def intify: Foo[Int]
         |   def stringListify(s: String*): Foo[String]
         |  }
         |
         |val foo: Foo[String] = ???
         |
         |val thingy: Foo[Int] = foo
         |  .intify
         |  .stringListify(
         |    "Hello",
         |    "World"
         |  )
         |  .stringListify(
         |    "Hello",
         |    "World"
         |  )
         |  .intify
         |  .intify
         |}
         |""".stripMargin,
      """|object Main{
         |  trait Foo[F] {
         |   def intify: Foo[Int]
         |   def stringListify(s: String*): Foo[String]
         |  }
         |
         |val foo: Foo[String] = ???
         |
         |val thingy: Foo[Int] = foo
         |  .intify/*: Foo<<(2:8)>>[Int<<scala/Int#>>]*/
         |  .stringListify(
         |    /*s = */"Hello",
         |    "World"
         |  )/*      : Foo<<(2:8)>>[String<<java/lang/String#>>]*/
         |  .stringListify(
         |    /*s = */"Hello",
         |    "World"
         |  )/*      : Foo<<(2:8)>>[String<<java/lang/String#>>]*/
         |  .intify/*: Foo<<(2:8)>>[Int<<scala/Int#>>]*/
         |  .intify/*: Foo<<(2:8)>>[Int<<scala/Int#>>]*/
         |}
         |""".stripMargin
    )

  @Test def `xray-long-chain-same-line` =
    check(
      """|object Main{
         |  trait Foo[F] {
         |   def intify: Foo[Int]
         |   def stringListify(s: String*): Foo[String]
         |  }
         |
         |val foo: Foo[String] = ???
         |
         |val thingy: Foo[Int] = foo
         |  .intify
         |  .stringListify(
         |    "Hello",
         |    "World"
         |  )
         |  .stringListify(
         |    "Hello",
         |    "World"
         |  )
         |  .intify.intify
         |}
         |""".stripMargin,
      """|object Main{
         |  trait Foo[F] {
         |   def intify: Foo[Int]
         |   def stringListify(s: String*): Foo[String]
         |  }
         |
         |val foo: Foo[String] = ???
         |
         |val thingy: Foo[Int] = foo
         |  .intify/*       : Foo<<(2:8)>>[Int<<scala/Int#>>]*/
         |  .stringListify(
         |    /*s = */"Hello",
         |    "World"
         |  )/*             : Foo<<(2:8)>>[String<<java/lang/String#>>]*/
         |  .stringListify(
         |    /*s = */"Hello",
         |    "World"
         |  )/*             : Foo<<(2:8)>>[String<<java/lang/String#>>]*/
         |  .intify.intify/*: Foo<<(2:8)>>[Int<<scala/Int#>>]*/
         |}
         |""".stripMargin
    )

  @Test def `xray-tikka-masala-curried` =
    check(
      """|object Main{
         |  trait Foo[F] {
         |   def intify: Foo[Int]
         |   def stringListify(s: String)(s2: String): Foo[String]
         |  }
         |
         |val foo: Foo[String] = ???
         |
         |val thingy: Foo[Int] = foo
         |  .intify
         |  .stringListify(
         |    "Hello"
         |  )(
         |    "World"
         |  )
         |  .stringListify(
         |    "Hello"
         |  )(
         |    "World"
         |  )
         |  .intify
         |  .intify
         |}
         |""".stripMargin,
      """|object Main{
         |  trait Foo[F] {
         |   def intify: Foo[Int]
         |   def stringListify(s: String)(s2: String): Foo[String]
         |  }
         |
         |val foo: Foo[String] = ???
         |
         |val thingy: Foo[Int] = foo
         |  .intify/*: Foo<<(2:8)>>[Int<<scala/Int#>>]*/
         |  .stringListify(
         |    /*s = */"Hello"
         |  )(
         |    /*s2 = */"World"
         |  )/*      : Foo<<(2:8)>>[String<<java/lang/String#>>]*/
         |  .stringListify(
         |    /*s = */"Hello"
         |  )(
         |    /*s2 = */"World"
         |  )/*      : Foo<<(2:8)>>[String<<java/lang/String#>>]*/
         |  .intify/*: Foo<<(2:8)>>[Int<<scala/Int#>>]*/
         |  .intify/*: Foo<<(2:8)>>[Int<<scala/Int#>>]*/
         |}
         |""".stripMargin
    )

  @Test def `xray-for-comprehension` =
    check(
      """|object Main{
         |trait Foo[A]{
         |  def flatMap[B](f: A => Foo[B]): Foo[B]
         |  def map[B](f: A => B): Foo[B]
         |  def bar(s: String): Foo[A]
         |}
         |val foo1: Foo[String] = ???
         |val foo2: Foo[Int] = ???
         |val result = for {
         | foo <- foo1
         | bar <- foo2
         |   .bar(s = foo)
         |   .bar(s = foo)
         |   .bar(s = foo)
         |} yield bar
         |}
         |""".stripMargin,
      """|object Main{
         |trait Foo[A]{
         |  def flatMap[B](f: A => Foo[B]): Foo[B]
         |  def map[B](f: A => B): Foo[B]
         |  def bar(s: String): Foo[A]
         |}
         |val foo1: Foo[String] = ???
         |val foo2: Foo[Int] = ???
         |val result/*: Foo<<(2:6)>>[Int<<scala/Int#>>]*/ = for {
         | foo <- foo1
         | bar <- foo2
         |   .bar(s = foo)/*: Foo<<(2:6)>>[Int<<scala/Int#>>]*/
         |   .bar(s = foo)/*: Foo<<(2:6)>>[Int<<scala/Int#>>]*/
         |   .bar(s = foo)/*: Foo<<(2:6)>>[Int<<scala/Int#>>]*/
         |} yield bar
         |}
         |""".stripMargin
    )

  @Test def `xray-metals-i7999` =
    check(
      """|object Main{
         |   case class User(
         |      name: String = {
         |         Map.toString
         |      }
         |   )
         |}
         |""".stripMargin,
      """|object Main{
         |   case class User(
         |      name: String = {
         |         Map.toString
         |      }
         |   )
         |}
         |""".stripMargin
    )

  @Test def `xray-metals-i8021` =
    check(
      """|object Main:
         |
         |  case class Order(id: String, amount: BigDecimal)
         |  case class User(name: String, orders: List[Order])
         |
         |  val users = List(
         |    User("Alice", List(Order("A1", 100), Order("A2", 50))),
         |    User("Bob", List(Order("B1", 200)))
         |  )
         |
         |  val total = users
         |    .filter(_.name.startsWith("A"))
         |    .flatMap(_.orders)
         |    .map(_.amount)
         |    .sum
         |""".stripMargin,
      """|object Main:
         |
         |  case class Order(id: String, amount: BigDecimal)
         |  case class User(name: String, orders: List[Order])
         |
         |  val users/*: List<<scala/collection/immutable/List#>>[User<<(4:13)>>]*/ = List/*[User<<(4:13)>>]*/(
         |    /*elems = */User(/*name = */"Alice", /*orders = */List/*[Order<<(3:13)>>]*/(/*elems = */Order(/*id = */"A1", /*int2bigDecimal<<scala/math/BigDecimal.int2bigDecimal().>>(*//*amount = */100/*)*/), Order(/*id = */"A2", /*int2bigDecimal<<scala/math/BigDecimal.int2bigDecimal().>>(*//*amount = */50/*)*/))),
         |    User(/*name = */"Bob", /*orders = */List/*[Order<<(3:13)>>]*/(/*elems = */Order(/*id = */"B1", /*int2bigDecimal<<scala/math/BigDecimal.int2bigDecimal().>>(*//*amount = */200/*)*/)))
         |  )
         |
         |  val total/*: BigDecimal<<scala/math/BigDecimal#>>*/ = users
         |    .filter(/*p = */_.name.startsWith("A"))/*: List<<scala/collection/immutable/List#>>[User<<(4:13)>>]*/
         |    .flatMap/*[Order<<(3:13)>>]*/(/*f = */_.orders)/*             : List<<scala/collection/immutable/List#>>[Order<<(3:13)>>]*/
         |    .map/*[BigDecimal<<scala/math/BigDecimal#>>]*/(/*f = */_.amount)/*                 : List<<scala/collection/immutable/List#>>[BigDecimal<<scala/math/BigDecimal#>>]*/
         |    .sum/*[BigDecimal<<scala/math/BigDecimal#>>]*//*(using BigDecimalIsFractional<<scala/math/Numeric.BigDecimalIsFractional.>>)*//*                           : BigDecimal<<scala/math/BigDecimal#>>*/
         |""".stripMargin
    )

  @Test def `implicit-params-metals-i8029` =
    check(
      """|trait Codec[T]
        |object Main {
        |  given intCodec: Codec[Int] = ???
        |  val x = summon[Codec[Int]]
        |}
        |""".stripMargin,
      """|trait Codec[T]
        |object Main {
        |  given intCodec: Codec[Int] = ???
        |  val x/*: Codec<<(1:6)>>[Int<<scala/Int#>>]*/ = summon[Codec[Int]]/*(using intCodec<<(3:8)>>)*/
        |}
        |""".stripMargin
    )

  @Test def `implicit-params-metals-i8029-2` =
    check(
      """|trait Codec[T]
        |object Main {
        |  implicit val intCodec: Codec[Int] = ???
        |  val x = implicitly[Codec[Int]]
        |  val y = summon[Codec[Int]]
        |}
        |""".stripMargin,
      """|trait Codec[T]
        |object Main {
        |  implicit val intCodec: Codec[Int] = ???
        |  val x/*: Codec<<(1:6)>>[Int<<scala/Int#>>]*/ = implicitly[Codec[Int]]/*(using intCodec<<(3:15)>>)*/
        |  val y/*: Codec<<(1:6)>>[Int<<scala/Int#>>]*/ = summon[Codec[Int]]/*(using intCodec<<(3:15)>>)*/
        |}
        |""".stripMargin
    )

  @Test def `closing-labels-1` =
    check(
      """|object Main{
         |  def bestNumber: Int = {
         |    234
         |  }
         |}
         |""".stripMargin,
      """|object Main{
         |  def bestNumber: Int = {
         |    234
         |  }/*bestNumber*/
         |}/*Main*/
         |""".stripMargin,
      closingLabels = true
    )

  @Test def `closing-labels-weird-formatting` =
    check(
      """|object Main{
         |  def bestNumber: Int = {
         |    def greatNumber: Long = {
         |      3
         |    }234}
         |}
         |""".stripMargin,
      """|object Main{
         |  def bestNumber: Int = {
         |    def greatNumber: Long = {
         |      3
         |    }/*greatNumber*/234}/*bestNumber*/
         |}/*Main*/
         |""".stripMargin,
      closingLabels = true
    )

  @Test def `closing-labels-inferred-type` =
    check(
      """|object Main{
         |  def bestNumber = {
         |    234
         |  }
         |}
         |""".stripMargin,
      """|object Main{
         |  def bestNumber/*: Int<<scala/Int#>>*/ = {
         |    234
         |  }/*bestNumber*/
         |}/*Main*/
         |""".stripMargin,
      closingLabels = true
    )
}
