import annotation.{precise, targetName}
import annotation.unchecked.uncheckedVariance
import language.implicitConversions

class Box[T](x: T)

object preciseDefs:
  def id[@precise T](x: T): T = x
  def idBox[@precise T](t: T): Box[T] = ???

  final val x = id(3)
  val xTest: 3 = x

  final val x3 = id(id(id(3)))
  val x3Test: 3 = x3

  final val tpl = id((1, 2))
  val tplTest: (1, 2) = tpl

  final val tpl2 = id((1, 2, (3, 4)))
  val tpl2Test: (1, 2, (3, 4)) = tpl2

  final val list1 = id(1 :: Nil)
  val list1Test: List[1] = list1

  final val list1hi = id(1 :: "hi" :: Nil)
  val list1hiTest: List[1 | "hi"] = list1hi

  val c: Boolean = ???
  val ifVal = idBox(if c then 2 else 3)
  val ifValTest: Box[2 | 3] = ifVal

  def huge[@precise T1, T2, T3, @precise T4, @precise T5](
    t1: T1, t2: T2, r: Int
  )(t3: T3, r2: Int, t4: T4)(t5: T5*): Box[(T1, T2, T3, T4, T5)] = ???

  val h1 = huge((1, 2), (3, 4), 5)((6, 7), 8, (9, 10))(11, 12)
  val h1Test: Box[((1, 2), (Int, Int), (Int, Int), (9, 10), 11 | 12)] = h1


object preciseUpperBound:
  def id[@precise TP, T <: TP](t: T): Box[T] = ???
  val x = id(1)
  val xTest: Box[1] = x


object preciseDepArgs:
  def same[@precise T](x: T, y: T): Box[T] = ???
  final val sameVal = same(1, 2)
  val sameValTest: Box[1 | 2] = sameVal

  def sameVarArgs[@precise T](a: T*): Box[T] = ???
  final val sameVal123hi = sameVarArgs(1, 2, 3, "hi")
  val sameVal123hiTest: Box[1 | 2 | 3 | "hi"] = sameVal123hi

  def dep2[T1, @precise T2 <: T1](t1: T1)(t2: T2): Box[T1] = ???
  final val d2 = dep2(1)(2)
  val d2Test: Box[Int] = d2

// TODO: revisit after fix for https://github.com/lampepfl/dotty/issues/15813
//  def dep1[@precise T1, T2 <: T1](t1: T1)(t2: T2): Box[T1] = ???
//  val d1 = dep1(1)(2)

// TODO: revisit after fix for https://github.com/lampepfl/dotty/issues/15813
//  def dep12[@precise T1, @precise T2 <: T1](t1: T1, t2: T2): Box[T1] = ???
//  final val d12 = dep12(1)(2)
//  val d12Test: Box[1 | 2] = d12


object preciseExtendClassArgs:
  class Foo[@precise T](val value: T)

  object Bar extends Foo(5)
  val barTest: 5 = Bar.value

  object Baz extends Foo((1, 2))
  val bazTest: (1, 2) = Baz.value


object preciseEnums:
  enum Foo[@precise T](val value: T):
    case Bar extends Foo(5)
    case Baz extends Foo((1, 2))

  import Foo.*
  val barTest: 5 = Bar.value
  val bazTest: (1, 2) = Baz.value


object preciseDefaultValues:
  def id[@precise T](x: T = 1): Box[T] = ???
  def np[T](x: T = 1): Box[T] = ???

  val x = id()
  val xTest: Box[1] = x
  val y = np()
  val yTest: Box[Int] = y

  def idTpl[@precise T](x: Int)(y: T = (1, (2, 3))): Box[T] = ???
  def npTpl[T](x: Int)(y: T = (1, (2, 3))): Box[T] = ???

  val xTpl = idTpl(22)()
  val xTplTest: Box[(1, (2, 3))] = xTpl
  val yTpl = npTpl(22)()
  val yTplTest: Box[(Int, (Int, Int))] = yTpl


object preciseGivens:
  given one: 1 = 1
  def fetch[@precise T <: Int](using T): Box[T] = ???
  val f = fetch
  val fTest: Box[1] = f

  class PreciseBox[@precise T]
  given [T]: PreciseBox[T] with {}

  def check[T](t: T)(using PreciseBox[T]): Box[T] = ???
  val c1 = check(1)
  val c1Test: Box[Int] = c1

  def foo[@precise T: Box](arg: T): Box[T] = summon[Box[T]]
  object fooTest:
    given Box[1] = new Box[1](1)
    val x = foo(1)
    val xTest: Box[1] = x


object preciseTypeParamPropagation:
  def idBox[@precise T](t: T): Box[T] = ???
  def idBoxBox[BB](x: Box[BB]): Box[BB] = x
  val bb1 = idBoxBox(idBox(1))
  val bb1Test: Box[1] = bb1


object preciseInvariance:
  class PreciseBox[@precise T](x: T)
  val c: Boolean = ???
  val b = PreciseBox(if c then 2 else 3)
  val bTest: PreciseBox[2 | 3] = b
  final val b3 = PreciseBox(PreciseBox(PreciseBox(3)))
  val b3Test: PreciseBox[PreciseBox[PreciseBox[3]]] = b3

  final val tpl = PreciseBox((1, (2, 3), "hi"))
  val tplTest: PreciseBox[(1, (2, 3), "hi")] = tpl

  final val tpl2: (1, (2, 3), "hi") = (1, (2, 3), "hi")
  final val tpl3 = PreciseBox(tpl2)
  val tpl3Test: PreciseBox[tpl2.type] = tpl3

  class Boxx[@precise T](x: T*)
  val b123 = Boxx(1, 2, 3)
  val b123Test: Boxx[1 | 2 | 3] = b123


object preciseCovariance:
  case class BoxC[@precise +A](x: A)
  def fromBox[B <: Any](x: BoxC[B]): BoxC[B] = x
  final val b1 = BoxC(1)
  val b11 = fromBox(b1)
  val b11Test: BoxC[1] = b11
  val b11CovTest: BoxC[Int] = b11

  class Inv[A, B]
  class BoxCI[@precise +C, +I](c: C, i: I)
  def fromBoxCI[C, I](x: BoxCI[C, I]): Inv[C, I] = ???
  val bci = BoxCI(1, 2)
  val bciTest: BoxCI[1, Int] = bci
  val fbci = fromBoxCI(bci)
  val fbciTest: Inv[1, Int] = fbci
  val fbci12 = fromBoxCI(??? : BoxCI[1, 2])
  val fbci12Test: Inv[1, Int] = fbci12

  class BoxIC[+I, @precise +C](i: I, c: C)
  def fromBoxIC[I, C](x: BoxIC[I, C]): Inv[I, C] = ???
  val bic = BoxIC(1, 2)
  val bicTest: BoxIC[Int, 2] = bic
  val fbic = fromBoxIC(bic)
  val fbicTest: Inv[Int, 2] = fbic
  val fbic12 = fromBoxIC(??? : BoxIC[1, 2])
  val fbic12Test: Inv[Int, 2] = fbic12


object preciseTypeAlias:
  case class BoxC[@precise +A](x: A)
  type BoxCC[A] = BoxC[A]
  def fromBox[B](x: BoxCC[B]): BoxCC[B] = x
  final val b1 = BoxC(1)
  val b11 = fromBox(b1)
  val b11Test: BoxCC[1] = b11
  val b11CovTest: BoxCC[Int] = b11


object preciseCovariantComposition:
  object direct:
    class BoxC[@precise +A]
    def fromBox[B](x: Box[BoxC[B]]): BoxC[B] = ???
    val bb1: Box[BoxC[1]] = ???
    val frombb1 = fromBox(bb1)
    val frombb1Test: BoxC[1] = frombb1

  object boring:
    class BoxC[@precise +A]
    def fromBox[B](x: BoxC[(1, B)]): BoxC[B] = ???
    val b1: BoxC[(1, 1)] = ???
    val fromb1 = fromBox(b1)
    val fromb1Test: BoxC[1] = fromb1


object preciseCovariantOpaque:
  opaque type BoxC[@precise +A] = A
  def fromBox[B <: Any](x: BoxC[B]): BoxC[B] = x
  val b1: BoxC[1] = ???
  val b11 = fromBox(b1)
  val b11Test: BoxC[1] = b11


object preciseContravariantGiven:
  object directPreciseInTC:
    trait TC[@precise -T]:
      type Out = Box[T @uncheckedVariance]
      val value: Out = ???
    object TC:
      given [T]: TC[T] = new TC[T]{}

    val b1 = summon[TC[1]]
    val b1Test: Box[1] = b1.value

  object directPreciseInGiven:
    trait TC[-T]:
      type Out = Box[T @uncheckedVariance]
      val value: Out = ???

    object TC:
      given fromInt[@precise T <: Int](using DummyImplicit): TC[T] = new TC[T] {}
      given fromString[T <: String]: TC[T] = new TC[T] {}

    val b1 = summon[TC[1]]
    val b1Test: Box[1] = b1.value
    val bHi = summon[TC["Hi"]]
    val bHiTest: Box[String] = bHi.value

  object secondaryGivens:
    trait TC2[@precise -T]:
      type Out = Box[T @uncheckedVariance]
      val value: Out = ???
    object TC2:
      given [T]: TC2[T] = new TC2[T] {}
    trait TC[-T]:
      type Out
      val value: Out = ???
    object TC:
      transparent inline given fromInt[@precise T <: Int](using tc2: TC2[T]): TC[T] = new TC[T]:
        type Out = tc2.Out

    val b1 = summon[TC[1]]
    val b1Test: Box[1] = b1.value

  object covariant:
    trait AbstractBox
    class BoxC[@precise +T] extends AbstractBox
    trait TC[-T]:
      type Out <: AbstractBox
      val value: Out = ???
    object TC:
      transparent inline given [T]: TC[BoxC[T]] = new TC[BoxC[T]]:
        type Out = BoxC[T]

    val b1 = summon[TC[BoxC[1]]]
    val b1Test: BoxC[1] = b1.value


object preciseExtDefs:
  object form1:
    extension [@precise T, V](t: T)
      def tester(v: V): Box[(T, V)] = ???
      def tester: Box[T] = ???

    val x = (1, 2).tester(3)
    val xTest: Box[((1, 2), Int)] = x

    val y = (1, 2).tester
    val yTest: Box[(1, 2)] = y

  object form2:
    extension[@precise T, @precise V] (t: T)
      def tester(v: V): Box[(T, V)] = ???
      def tester: Box[T] = ???

    val x = (1, 2).tester(3)
    val xTest: Box[((1, 2), 3)] = x

    val y = (1, 2).tester
    val yTest: Box[(1, 2)] = y

  object form3:
    extension[T, @precise V] (t: T)
      def tester(v: V): Box[(T, V)] = ???
      def tester: Box[T] = ???

    val x = (1, 2).tester(3)
    val xTest: Box[((Int, Int), 3)] = x

    val y = (1, 2).tester
    val yTest: Box[(Int, Int)] = y

  object form4:
    extension [@precise T](t: T)
      def testerp[@precise V](v: V): Box[(T, V)] = ???
      def testernp[V](v: V): Box[(T, V)] = ???

    val x = (1, 2).testerp(3)
    val xTest: Box[((1, 2), 3)] = x

    val y = (1, 2).testernp(3)
    val yTest: Box[((1, 2), Int)] = y


object preciseImplicitClasses:
  object form1:
    implicit class Ext[@precise T](t: T):
      def tester[@precise V](v: V): Box[(T, V)] = ???
      def tester: Box[T] = ???

    val x = (1, 2).tester(3)
    val xTest: Box[((1, 2), 3)] = x

    val y = (1, 2).tester
    val yTest: Box[(1, 2)] = y

  object form2:
    implicit class Ext[@precise T](t: T):
      def tester[V](v: V): Box[(T, V)] = ???

    val x = (1, 2).tester(3)
    val xTest: Box[((1, 2), Int)] = x

  object form3:
    implicit class Ext[T](t: T):
      def tester[@precise V](v: V): Box[(T, V)] = ???

    val x = (1, 2).tester(3)
    val xTest: Box[((Int, Int), 3)] = x


object preciseCovarianceWithCompiletimeOps:
  import compiletime.ops.int.+
  class Inlined[@precise +T <: Int]
  extension [T <: Int](lhs: Inlined[T])
    def inc: Inlined[T + 1] = ???

  val i1 = Inlined[1]
  val i3: Inlined[3] = i1.inc.inc


object preciseByName:
  def id[@precise T](t: => T): Box[T] = ???
  val x = id(1)
  val xTest: Box[1] = x
  val y = id((1, 2))
  val yTest: Box[(1, 2)] = y


object preciseFuncX:
  object func0:
    def id[@precise T](t: () => T): Box[T] = ???
    val x = id(() => 1)
    val xTest: Box[1] = x
    val y = id(() => (1, 2))
    val yTest: Box[(1, 2)] = y

  object func2:
    def id[@precise T](t: (Int, Int) => T): Box[T] = ???
    val y = id((a, b) => (1, 2))
    val yTest: Box[(1, 2)] = y


object preciseOverloading:
  @targetName("intID")
  def id[@precise T <: Int](t: T): Box[T] = ???
  @targetName("stringID")
  def id[T <: String](t: T): Box[T] = ???
  @targetName("longID")
  def id[@precise T <: Long](t: T)(using T =:= 1L): Box[T] = ???

  val i1 = id(1)
  val i1Test: Box[1] = i1
  val iOne = id("One")
  val iOneTest: Box[String] = iOne
  val i1L = id(1L)
  val i1LTest: Box[1L] = i1L
  val x = id(2L) // error

  class Foo:
    def ==[@precise R](r: R): Box[R] = ???

  val f = new Foo
  val x = f == ((1, 2), 3)
  val xTest: Box[((1, 2), 3)] = x


object preciseImplicitConversion:
  object normalDefs:
    implicit def toBoxFromTuple[@precise T <: Tuple](from: T): Box[T] = Box(from)
    implicit def toBoxFromInt[@precise T <: Int](from: T): Box[T] = Box(from)
    implicit def toBoxFromString[T <: String](from: T): Box[T] = Box(from)
    def box[T1, T2, T3, @precise T4, T5](
      b1: Box[T1], b2: Box[T2], r: Int, b3: Box[T3]
    )(
      r2: Int, b4: Box[T4], b5: Box[T5]
    ): Box[(T1, T2, T3, T4, T5)] = ???

    final val b1 = box((1, 2), (3, 4), 999, 4)(999, "two", 22)
    val b1Test: Box[((1, 2), (3, 4), 4, "two", 22)] = b1

    final val b2 = box("one", (1, 2), 999, 3)(999, ("hi", "there"), "bye")
    val b2Test: Box[(String, (1, 2), 3, ("hi", "there"), String)] = b2

  object transparentInline1:
    trait OutBox[T]{ type Out }
    object OutBox:
      transparent inline implicit def conv[T, @precise V](value: V): OutBox[T] =
        new OutBox[T]{ type Out = V }

    def out[T](ob: OutBox[T]): ob.Out = ???

    val x = out((0, 1))
    val xTest: (0, 1) = x

  object transparentInline2:
    trait OutBox[T]{ type Out }
    object OutBox:
      transparent inline implicit def conv[T, @precise V](inline value: V): OutBox[T] =
        new OutBox[T]{ type Out = V }

    def out[T](ob: OutBox[T]): ob.Out = ???

    val x = out((0, 1))
    val xTest: (0, 1) = x


object preciseImplicitConversionNewStyle:
  given toBoxFromTuple[@precise T <: Tuple]: Conversion[T, Box[T]] = Box(_)
  given toBoxFromInt[@precise T <: Int]: Conversion[T, Box[T]] = Box(_)
  given toBoxFromString[T <: String]: Conversion[T, Box[T]] = Box(_)
  def box[T1, T2, T3, @precise T4, T5](
    b1: Box[T1], b2: Box[T2], r: Int, b3: Box[T3]
  )(
    r2: Int, b4: Box[T4], b5: Box[T5]
  ): Box[(T1, T2, T3, T4, T5)] = ???

  final val b1 = box((1, 2), (3, 4), 999, 4)(999, "two", 22)
  val b1Test: Box[((1, 2), (3, 4), 4, "two", 22)] = b1

  final val b2 = box("one", (1, 2), 999, 3)(999, ("hi", "there"), "bye")
  val b2Test: Box[(String, (1, 2), 3, ("hi", "there"), String)] = b2


object precisePolymorphicTypesAndValues:
  type Id1 = [@precise T] => T => Box[T]
  val id1Check: Id1 = [T] => (t: T) => Box(t) // error

  type Id2 = [T] => T => Box[T]
  val id2Check: Id2 = [@precise T] => (t: T) => Box(t) // error


object preciseBlock:
  def id[@precise T](x: T): Box[T] = ???
  def np[T](x: T): Box[T] = ???

  val x = id({
    val y = id(1)
    val yTest: Box[1] = y
    1
  })
  val xTest: Box[1] = x

  val x2 = id({
    val y = np(1)
    val yTest: Box[Int] = y
    1
  })
  val x2Test: Box[1] = x2


object preciseTupleParams:
  object p_p:
    def id[@precise TT1, @precise TT2](t: (TT1, TT2)): (TT1, TT2) = ???
    val x = id(((0, 1), 2))
    val xTest: ((0, 1), 2) = x
