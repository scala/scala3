import dotty.generic._

object Syntax {
  type &:[H, T <: Prod] = PCons[H, T]
  type |:[H, T <: Sum] = SCons[H, T]

  implicit class ProdSyntax1[T <: Prod](t: T) extends AnyVal {
    def &:[H](h: H): PCons[H, T] = PCons(h, t)
  }
}

import Syntax._

object RepresentableTestsAux {
  sealed trait Fruit
  case class Apple() extends Fruit
  case class Banana() extends Fruit
  case class Orange() extends Fruit
  case class Pear() extends Fruit

  sealed trait AbstractSingle
  case class Single() extends AbstractSingle

  sealed trait Tree[T]
  case class Node[T](left: Tree[T], right: Tree[T]) extends Tree[T]
  case class Leaf[T](t: T) extends Tree[T]

  sealed trait Enum
  case object A extends Enum
  case object B extends Enum
  case object C extends Enum

  sealed trait L
  case object N extends L
  case class C(hd: Int, tl: L) extends L

  case class Company(depts : List[Dept])
  sealed trait Subunit
  case class Dept(name : String, manager : Employee, subunits : List[Subunit]) extends Subunit
  case class Employee(person : Person, salary : Salary) extends Subunit
  case class Person(name : String, address : String, age: Int)

  case class Salary(salary : Double)

  case class PersonWithPseudonims(name: String, nicks: String*)

  case class PersonWithPseudonimsT[T](name: T, nicks: T*)

  // NOT SUPPORTED
  // sealed trait AbstractNonCC
  // class NonCCA(val i: Int, val s: String) extends AbstractNonCC
  // class NonCCB(val b: Boolean, val d: Double) extends AbstractNonCC
  // class NonCCWithVars(var c: Char, var l: Long) extends AbstractNonCC

  // class NonCCWithCompanion private (val i: Int, val s: String)
  // object NonCCWithCompanion {
  //   def apply(i: Int, s: String) = new NonCCWithCompanion(i, s)
  //   def unapply(s: NonCCWithCompanion): Option[(Int, String)] = Some((s.i, s.s))
  // }

  class NonCCLazy(prev0: => NonCCLazy, next0: => NonCCLazy) {
    lazy val prev = prev0
    lazy val next = next0
  }

  sealed trait Xor[+A, +B]
  case class Left[+LA](a: LA) extends Xor[LA, Nothing]
  case class Right[+RB](b: RB) extends Xor[Nothing, RB]

  sealed trait Base[BA, BB]
  case class Swap[SA, SB](a: SA, b: SB) extends Base[SB, SA]

  sealed trait Overlapping
  sealed trait OA extends Overlapping
  case class OAC(s: String) extends OA
  sealed trait OB extends Overlapping
  case class OBC(s: String) extends OB
  case class OAB(i: Int) extends OA with OB
}

object RepresentableTests {
  import RepresentableTestsAux._

  type APBO = Apple |: Banana |: Orange |: Pear |: SNil
  type ABC = A.type |: B.type |: C.type |: SNil

  def testProductBasics(): Unit = {
    val p = Person("Joe Soap", "Brighton", 23)
    type SSI = String &: String &: Int &: PNil
    val gen = Representable[Person]

    val p0 = gen.to(p)
    identity[SSI](p0)
    assert(("Joe Soap" &: "Brighton" &: 23 &: PNil()) == p0)

    val p1 = gen.from(p0)
    identity[Person](p1)
    assert(p == p1)
  }

  def testProductVarargs(): Unit = {
    val p = PersonWithPseudonims("Joe Soap", "X", "M", "Z")
    val gen = Representable[PersonWithPseudonims]

    val p0 = gen.to(p)
    // identity[String &: Seq[String] &: PNil](p0)
    assert(("Joe Soap" &: Seq("X", "M", "Z") &: PNil()) == p0)

    val p1 = gen.from(p0)
    identity[PersonWithPseudonims](p1)
    assert(p == p1)
  }

  def testTuples(): Unit = {
    val gen1 = Representable[Tuple1[Int]]
    identity[Representable[Tuple1[Int]] { type Repr = Int &: PNil }](gen1)

    val gen2 = Representable[(Int, String)]
    identity[Representable[(Int, String)] { type Repr = Int &: String &: PNil }](gen2)

    val gen3 = Representable[(Int, String, Boolean)]
    identity[Representable[(Int, String, Boolean)] { type Repr = Int &: String &: Boolean &: PNil }](gen3)
  }

  def testCoproductBasics(): Unit = {
    val a: Fruit = Apple()
    val p: Fruit = Pear()
    val b: Fruit = Banana()
    val o: Fruit = Orange()

    val gen = Representable[Fruit]

    val a0 = gen.to(a)
    identity[APBO](a0)

    val p0 = gen.to(p)

    identity[APBO](p0)

    val b0 = gen.to(b)

    identity[APBO](b0)

    val o0 = gen.to(o)

    identity[APBO](o0)

    val a1 = gen.from(a0)
    identity[Fruit](a1)

    val p1 = gen.from(p0)
    identity[Fruit](p1)

    val b1 = gen.from(b0)
    identity[Fruit](b1)

    val o1 = gen.from(o0)
    identity[Fruit](o1)
  }

  def testSingletonCoproducts(): Unit = {
    type S = Single

    val gen = Representable[AbstractSingle]

    val s: AbstractSingle = Single()

    val s0 = gen.to(s)
    identity[(Single |: SNil)](s0)

    val s1 = gen.from(s0)
    identity[AbstractSingle](s1)
  }

  // TODO
  // def testOverlappingCoproducts(): Unit = {
  //   val gen = Representable[Overlapping]
  //   val o: Overlapping = OAB(1)
  //   val o0 = gen.to(o)
  //   typed[(OAB |: OAC |: OBC |: CNil)](o0)

  //   val o1 = gen.from(o0)
  //   typed[Overlapping](o1)
  // }

  def testCaseObjects(): Unit = {
    val a: Enum = A
    val b: Enum = B
    val c: Enum = C

    val gen = Representable[Enum]

    val a0 = gen.to(a)
    identity[ABC](a0)

    val b0 = gen.to(b)
    identity[ABC](b0)

    val c0 = gen.to(c)
    identity[ABC](c0)

    val a1 = gen.from(a0)
    identity[Enum](a1)

    val b1 = gen.from(b0)
    identity[Enum](b1)

    val c1 = gen.from(c0)
    identity[Enum](c1)
  }

  def testParametrized(): Unit = {
    val t: Tree[Int] = Node(Node(Leaf(23), Leaf(13)), Leaf(11))
    type NI = Node[Int] |: Leaf[Int] |: SNil

    val gen = Representable[Tree[Int]]

    val t0 = gen.to(t)
    identity[NI](t0)

    val t1 = gen.from(t0)
    identity[Tree[Int]](t1)
  }

  def testParametrizedWithVarianceOption(): Unit = {
    val o: Option[Int] = Option(23)
    type SN = None.type |: Some[Int] |: SNil

    val gen = Representable[Option[Int]]

    val o0 = gen.to(o)
    identity[SN](o0)

    val o1 = gen.from(o0)
    identity[Option[Int]](o1)
  }

  // TODO ???
  // def testParametrizedWithVarianceList(): Unit = {
  //   val l: List[Int] = List(1, 2, 3)
  //   type CN = ::[Int] |: Nil.type |: SNil

  //   val gen = Representable[List[Int]]

  //   val l0 = gen.to(l)
  //   identity[CN](l0)

  //   val l1 = gen.from(l0)
  //   identity[List[Int]](l1)
  // }

  def testParametrzedSubset(): Unit = {
    val l = Left(23)
    val r = Right(true)
    type IB = Left[Int] |: Right[Boolean] |: SNil

    val gen = Representable[Xor[Int, Boolean]]

    val c0 = gen.to(l)
    val d0 = SLeft(l)
    identity[IB](c0)
    assert(d0 == c0)

    val c1 = gen.to(r)
    val d1 = SRight(SLeft(r))
    identity[IB](c1)
    assert(d1 == c1)
  }

  def testParametrizedPermute(): Unit = {
    val s = Swap(23, true)
    type IB = Swap[Int, Boolean] |: SNil

    val gen = Representable[Base[Boolean, Int]]

    val s0 = gen.to(s)
    val s1 = SLeft(s)
    identity[IB](s0)
    assert(s1 == s0)
  }

  // NOT SUPPORTED
  // def testAbstractNonCC(): Unit = {
  //   val ncca = new NonCCA(23, "foo")
  //   val nccb = new NonCCB(true, 2.0)
  //   val nccc = new NonCCWithVars('c', 42)
  //   val ancc: AbstractNonCC = ncca

  //   val genA = Representable[NonCCA]
  //   val genB = Representable[NonCCB]
  //   val genC = Representable[NonCCWithVars]
  //   val genAbs = Representable[AbstractNonCC]

  //   val rA = genA.to(ncca)
  //   assertTypedEquals[Int &: String &: PNil](23 &: "foo" &: PNil, rA)

  //   val rB = genB.to(nccb)
  //   assertTypedEquals[Boolean &: Double &: PNil](true &: 2.0 &: PNil, rB)

  //   val rC = genC.to(nccc)
  //   assertTypedEquals[Char &: Long &: PNil]('c' &: 42l &: PNil, rC)

  //   val rAbs = genAbs.to(ancc)
  //   assertTypedEquals[NonCCA |: NonCCB |: NonCCWithVars |: SNil](SLeft(ncca), rAbs)

  //   val fA = genA.from(13 &: "bar" &: PNil)
  //   identity[NonCCA](fA)
  //   assert(13 == fA.i)
  //   assert("bar" == fA.s)

  //   val fB = genB.from(false &: 3.0 &: PNil)
  //   identity[NonCCB](fB)
  //   assert(false == fB.b)
  //   assert(3.0 == fB.d) // , Double.MinPositiveValue)

  //   val fC = genC.from('k' &: 313l &: PNil)
  //   identity[NonCCWithVars](fC)
  //   assert('k' == fC.c)
  //   assert(313l == fC.l)

  //   val fAbs = genAbs.from(SRight(SLeft(nccb)))
  //   identity(fAbs: AbstractNonCC) // Typed?
  //   assertTrue(fAbs.isInstanceOf[NonCCB])
  //   assert(true == fAbs.asInstanceOf[NonCCB].b)
  //   assert(2.0 == fAbs.asInstanceOf[NonCCB].d) // , Double.MinPositiveValue)
  // }

  // NOT SUPPORTED
  // def testNonCCWithCompanion(): Unit = {
  //   val nccc = NonCCWithCompanion(23, "foo")

  //   val gen = Representable[NonCCWithCompanion]

  //   val r = gen.to(nccc)
  //   assertTypedEquals[Int &: String &: PNil](23 &: "foo" &: PNil, r)

  //   val f = gen.from(13 &: "bar" &: PNil)
  //   identity(f: NonCCWithCompanion) // Typed?
  //   assert(13 == f.i)
  //   assert("bar" == f.s)
  // }

  // NOT SUPPORTED
  // def testNonCCLazy(): Unit = {
  //   lazy val (a: NonCCLazy, b: NonCCLazy, c: NonCCLazy) =
  //     (new NonCCLazy(c, b), new NonCCLazy(a, c), new NonCCLazy(b, a))

  //   val gen = Representable[NonCCLazy]

  //   val rB = gen.to(b)
  //   assertTypedEquals[NonCCLazy &: NonCCLazy &: PNil](a &: c &: PNil, rB)

  //   val fD = gen.from(a &: c &: PNil)
  //   identity[NonCCLazy](fD)
  //   assert(a == fD.prev)
  //   assert(c == fD.next)
  // }

  trait Parent {
    case class Nested(i: Int, s: String)

    sealed abstract class Foo extends Product with Serializable

    case object A extends Foo
    case object B extends Foo
    case class C() extends Foo
  }

  trait Child extends Parent { self =>
    val gen = Representable[Nested]
    val adtGen = Representable[Foo]
  }

  object O extends Child

  def testNestedInherited(): Unit = {
    val n0 = O.Nested(23, "foo")
    val repr = O.gen.to(n0)
    identity[(Int &: String &: PNil)](repr)
    val n1 = O.gen.from(repr)
    identity[O.Nested](n1)
    assert(n0 == n1)

    {
      val foo0 = O.B
      val repr = O.adtGen.to(foo0)
      identity[(O.A.type |: O.B.type |: O.C |: SNil)](repr)
    }

    // {
    //   val foo0 = O.C()
    //   val repr = O.adtGen.to(foo0)
    //   identity[(O.A.type |: O.B.type |: O.C |: SNil)](repr)
    // }
  }

  def testNonRepresentable(): Unit = {
    import scala.implicits.Not

    implicitly[Not[Representable[Int]]]
    implicitly[Not[Representable[Array[Int]]]]
    implicitly[Not[Representable[String]]]
    implicitly[Not[Representable[PNil]]]
    implicitly[Not[Representable[(Int &: String &: PNil)]]]
    implicitly[Not[Representable[SNil]]]
    implicitly[Not[Representable[(Int |: String |: SNil)]]]
  }

  sealed trait Color
  case object Green extends Color
  object Color {
    case object Red extends Color
  }

  def testNestedCaseObjects(): Unit = {
    val a: Option[Green.type] = None
    Representable[Green.type]
    Representable[Color.Red.type]
    // LabelledRepresentable[Green.type]
    // LabelledRepresentable[Color.Red.type]
  }

  sealed trait Base1
  case object Foo1 extends Base1
  case object Bar1 extends Base1

  trait TC[T]

  object TC {
    def apply[T](implicit tc: TC[T]): TC[T] = tc

    implicit def hnilTC: TC[PNil] = new TC[PNil] {}
    implicit def hconsTC[H, T <: Prod](implicit hd: => TC[H], tl: => TC[T]): TC[H &: T] = new TC[H &: T] {}

    implicit def cnilTC: TC[SNil] = new TC[SNil] {}
    implicit def cconsTC[H, T <: Sum](implicit hd: => TC[H], tl: => TC[T]): TC[H |: T] = new TC[H |: T] {}

    implicit def projectTC[F, G](implicit gen: Representable[F] { type Repr = G }, tc: => TC[G]): TC[F] = new TC[F] {}
  }

  def testCaseObjectsAndLazy(): Unit = {
    TC[Base1]
  }
}

// object RepresentableTestsAux2 {
//   sealed trait Foo[T]

//   object Foo {
//     implicit def derivePNil: Foo[PNil] = ???

//     implicit def deriveRepresentable[A, Rec <: Prod]
//       (implicit gen: Representable[A] { type Repr = Rec }, auto: Foo[Rec]): Foo[A] = ???
//   }

//   sealed class Bar[A]

//   object Bar extends Bar0 {
//     implicit def cnil: Bar[SNil] = ???
//   }

//   trait Bar0 {
//     implicit def deriveCoproduct[H, T <: Sum]
//       (implicit headFoo: Foo[H], tailAux: Bar[T]): Bar[(H |: T)] = ???

//     implicit def representable[A, U <: Sum]
//       (implicit gen: Representable[A] { type Repr = U }, auto: Bar[U]): Bar[A] = ???
//   }

//   class Outer1 {
//     sealed trait Color
//     object Inner {
//       case object Red extends Color
//     }

//     val r = Representable[Bar[Color]]
//     Bar.representable(r, implicitly)

//     implicitly[Bar[Color]]
//   }

  // object Outer2 {
  //   class Wrapper {
  //     sealed trait Color
  //   }
  //   val wrapper = new Wrapper
  //   import wrapper.Color
  //   case object Red extends Color
  //   case object Green extends Color
  //   case object Blue extends Color

  //   implicitly[Bar[Color]]
  // }

  // object Outer3 {
  //   class Wrapper {
  //     sealed trait Color
  //   }
  //   val wrapper = new Wrapper
  //   case object Red extends wrapper.Color
  //   case object Green extends wrapper.Color
  //   case object Blue extends wrapper.Color

  //   implicitly[Bar[wrapper.Color]]
  // }

  // object Outer4 {
  //   val wrapper = new Wrapper
  //   case object Red extends wrapper.Color
  //   case object Green extends wrapper.Color
  //   case object Blue extends wrapper.Color

  //   class Wrapper {
  //     sealed trait Color
  //     implicitly[Bar[wrapper.Color]]
  //   }
  // }

  // object Outer5 {
  //   trait Command
  //   object Command {
  //     sealed trait Execution extends Command
  //   }

  //   case class Buzz() extends Command.Execution
  //   case class Door() extends Command.Execution

  //   Representable[Command.Execution]
  // }
// }

// object MixedCCNonCCNested {
//   // Block local
//   {
//     object T1{
//       sealed abstract class Tree
//       final case class Node(left: Tree, right: Tree, v: Int) extends Tree
//       case object Leaf extends Tree
//     }

//     Representable[T1.Tree]
//     import T1._
//     Representable[Tree]

//     sealed trait A
//     sealed case class B(i: Int, s: String) extends A
//     case object C extends A
//     sealed trait D extends A
//     final case class E(a: Double, b: Option[Float]) extends D
//     case object F extends D
//     sealed abstract class Foo extends D
//     case object Baz extends Foo
//     final class Bar extends Foo
//     final class Baz(val i1: Int, val s1: String) extends Foo

//     Representable[A]
//     Representable[B]
//     Representable[C.type]
//     Representable[D]
//     Representable[E]
//     Representable[F.type]
//     Representable[Foo]
//     Representable[Baz.type]
//     Representable[Bar]
//     Representable[Baz]
//   }

//   def methodLocal: Unit = {
//     object T1{
//       sealed abstract class Tree
//       final case class Node(left: Tree, right: Tree, v: Int) extends Tree
//       case object Leaf extends Tree
//     }

//     Representable[T1.Tree]
//     import T1._
//     Representable[Tree]

//     sealed trait A
//     sealed case class B(i: Int, s: String) extends A
//     case object C extends A
//     sealed trait D extends A
//     final case class E(a: Double, b: Option[Float]) extends D
//     case object F extends D
//     sealed abstract class Foo extends D
//     case object Baz extends Foo
//     final class Bar extends Foo
//     final class Baz(val i1: Int, val s1: String) extends Foo

//     Representable[A]
//     Representable[B]
//     Representable[C.type]
//     Representable[D]
//     Representable[E]
//     Representable[F.type]
//     Representable[Foo]
//     Representable[Baz.type]
//     Representable[Bar]
//     Representable[Baz]
//   }

//   // Top level
//   object T1{
//     sealed abstract class Tree
//     final case class Node(left: Tree, right: Tree, v: Int) extends Tree
//     case object Leaf extends Tree
//   }

//   Representable[T1.Tree]
//   import T1._
//   Representable[Tree]

//   sealed trait A
//   sealed case class B(i: Int, s: String) extends A
//   case object C extends A
//   sealed trait D extends A
//   final case class E(a: Double, b: Option[Float]) extends D
//   case object F extends D
//   sealed abstract class Foo extends D
//   case object Baz extends Foo
//   final class Bar extends Foo
//   final class Baz(val i1: Int, val s1: String) extends Foo

//   Representable[A]
//   Representable[B]
//   Representable[C.type]
//   Representable[D]
//   Representable[E]
//   Representable[F.type]
//   Representable[Foo]
//   Representable[Baz.type]
//   Representable[Bar]
//   Representable[Baz]
// }

object EnumDefns1 {
  sealed trait EnumVal
  object BarA extends EnumVal { val name = "A" }
  object BarB extends EnumVal { val name = "B" }
  object BarC extends EnumVal { val name = "C" }
}

object EnumDefns2 {
  sealed trait EnumVal
  case object BarA extends EnumVal { val name = "A" }
  case object BarB extends EnumVal { val name = "B" }
  case object BarC extends EnumVal { val name = "C" }
}

object EnumDefns5 {
  sealed trait EnumVal
  object EnumVal {
    object BarA extends EnumVal { val name = "A" }
    object BarB extends EnumVal { val name = "B" }
    object BarC extends EnumVal { val name = "C" }
  }
}

object EnumDefns6 {
  sealed trait EnumVal
  object EnumVal {
    case object BarA extends EnumVal { val name = "A" }
    case object BarB extends EnumVal { val name = "B" }
    case object BarC extends EnumVal { val name = "C" }
  }
}

object TestEnum {
  // NOT SUPPORTED
  // object EnumDefns0 {
  //   sealed trait EnumVal
  //   val BarA = new EnumVal { val name = "A" }
  //   val BarB = new EnumVal { val name = "B" }
  //   val BarC = new EnumVal { val name = "C" }
  // }

  // def testEnum0(): Unit = {
  //   import EnumDefns0._

  //   val gen = Representable[EnumVal]
  //   val a0 = gen.to(BarA)
  //   assert(a0 == SLeft(BarA))

  //   val b0 = gen.to(BarB)
  //   assert(b0 == SRight(SLeft(BarB)))

  //   val c0 = gen.to(BarC)
  //   assert(c0 == SRight(SRight(SLeft(BarC))))
  // }

  def testEnum1(): Unit = {
    import EnumDefns1._

    val gen = Representable[EnumVal]
    val a0 = gen.to(BarA)
    val a1 = SLeft[BarA.type, SNil](BarA)
    assert(a0 == a1)

    val b0 = gen.to(BarB)
    val b1 = SRight(SLeft[BarB.type, SNil](BarB))
    assert(b0 == b1)

    val c0 = gen.to(BarC)
    val c1 = SRight(SRight(SLeft[BarC.type, SNil](BarC)))
    assert(c0 == c1)
  }

  def testEnum2(): Unit = {
    import EnumDefns2._

    val gen = Representable[EnumVal]
    val a0 = gen.to(BarA)
    val a1 = SLeft[BarA.type, SNil](BarA)
    assert(a0 == a1)

    val b0 = gen.to(BarB)
    val b1 = SRight(SLeft[BarB.type, SNil](BarB))
    assert(b0 == b1)

    val c0 = gen.to(BarC)
    val c1 = SRight(SRight(SLeft[BarC.type, SNil](BarC)))
    assert(c0 == c1)
  }

  // NOT SUPPORTED
  // object EnumDefns3 {
  //   sealed trait EnumVal
  //   val BarA, BarB, BarC = new EnumVal {}
  // }

  // def testEnum3(): Unit = {
  //   import EnumDefns3._

  //   val gen = Representable[EnumVal]
  //   val a0 = gen.to(BarA)
  //   assert(a0 == SLeft(BarA))

  //   val b0 = gen.to(BarB)
  //   assert(b0 == SRight(SLeft(BarB)))

  //   val c0 = gen.to(BarC)
  //   assert(c0 == SRight(SRight(SLeft(BarC))))
  // }

  // NOT SUPPORTED
  // object EnumDefns4 {
  //   sealed trait EnumVal
  //   object EnumVal {
  //     val BarA = new EnumVal { val name = "A" }
  //     val BarB = new EnumVal { val name = "B" }
  //     val BarC = new EnumVal { val name = "C" }
  //   }
  // }

  // def testEnum4(): Unit = {
  //   import EnumDefns4._
  //   import EnumVal._

  //   val gen = Representable[EnumVal]
  //   val a0 = gen.to(BarA)
  //   assert(a0 == SLeft(BarA))

  //   val b0 = gen.to(BarB)
  //   assert(b0 == SRight(SLeft(BarB)))

  //   val c0 = gen.to(BarC)
  //   assert(c0 == SRight(SRight(SLeft(BarC))))
  // }

  def testEnum5(): Unit = {
    import EnumDefns5._
    import EnumVal._

    val gen = Representable[EnumVal]
    val a0 = gen.to(BarA)
    val a1 = SLeft[BarA.type, SNil](BarA)
    assert(a0 == a1)

    val b0 = gen.to(BarB)
    val b1 = SRight(SLeft[BarB.type, SNil](BarB))
    assert(b0 == b1)

    val c0 = gen.to(BarC)
    val c1 = SRight(SRight(SLeft[BarC.type, SNil](BarC)))
    assert(c0 == c1)
  }

  def testEnum6(): Unit = {
    import EnumDefns6._
    import EnumVal._

    val gen = Representable[EnumVal]
    val a0 = gen.to(BarA)
    val a1 = SLeft[BarA.type, SNil](BarA)
    assert(a0 == a1)

    val b0 = gen.to(BarB)
    val b1 = SRight(SLeft[BarB.type, SNil](BarB))
    assert(b0 == b1)

    val c0 = gen.to(BarC)
    val c1 = SRight(SRight(SLeft[BarC.type, SNil](BarC)))
    assert(c0 == c1)
  }

  // NOT SUPPORTED
  // object EnumDefns7 {
  //   sealed trait EnumVal
  //   object EnumVal {
  //     val BarA, BarB, BarC = new EnumVal {}
  //   }
  // }

  // def testEnum7(): Unit = {
  //   import EnumDefns7._
  //   import EnumVal._

  //   val gen = Representable[EnumVal]
  //   val a0 = gen.to(BarA)
  //   assert(a0 == SLeft(BarA))

  //   val b0 = gen.to(BarB)
  //   assert(b0 == SRight(SLeft(BarB)))

  //   val c0 = gen.to(BarC)
  //   assert(c0 == SRight(SRight(SLeft(BarC))))
  // }
}

object TestPrefixes1 {
  trait Defs {
    case class CC(i: Int, s: String)

    sealed trait Sum
    case class SumI(i: Int) extends Sum
    case class SumS(s: String) extends Sum
  }

  object Defs extends Defs

  object Derivations {
    Representable[Defs.CC]
    Representable[Defs.SumI]
    Representable[Defs.SumS]

    Representable[Defs.Sum]
    // Representable.materialize[Defs.Sum, Defs.SumI |: Defs.SumS |: SNil]
  }
}

// #3564, should work otherwise
// package TestSingletonMembers {
//   case class CC(i: Int, s: "msg")

//   object Derivations2 {
//     Representable[CC]
//   }
// }

// TRICKY, there is no infrastructure to get "reachable" children, see #3574
// object PathVariantDefns {
//   sealed trait AtomBase {
//     sealed trait Atom
//     case class Zero(value: String) extends Atom
//   }

//   trait Atom1 extends AtomBase {
//     case class One(value: String) extends Atom
//   }

//   trait Atom2 extends AtomBase {
//     case class Two(value: String) extends Atom
//   }

//   object Atoms01 extends AtomBase with Atom1
//   object Atoms02 extends AtomBase with Atom2
// }

// object PathVariants {
//   import PathVariantDefns._

//   val gen1 = Representable[Atoms01.Atom]
//   implicitly[gen1.Repr =:= (Atoms01.One |: Atoms01.Zero |: SNil)]

//   val gen2 = Representable[Atoms02.Atom]
//   implicitly[gen2.Repr =:= (Atoms02.Two |: Atoms02.Zero |: SNil)]
// }

object PrivateCtorDefns {
  sealed trait PublicFamily
  case class PublicChild() extends PublicFamily
  private case class PrivateChild() extends PublicFamily
}

object PrivateCtor {
  import PrivateCtorDefns._

  // illTyped Representable[PublicFamily]
}

object Thrift {
  object TProduct {
    def apply(a: Double, b: String): TProduct = new Immutable(a, b)

    def unapply(tp: TProduct): Option[Product2[Double, String]] = Some(tp)

    // class Immutable(val a: Double, val b: String) extends TProduct

    class Immutable(
      val a: Double,
      val b: String,
      val _passthroughFields: scala.collection.immutable.Map[Short, Byte]
    ) extends TProduct {
      def this(
        a: Double,
        b: String
      ) = this(
        a,
        b,
        Map.empty
      )
    }
  }

  trait TProduct extends Product2[Double, String] {
    def a: Double
    def b: String

    def _1 = a
    def _2 = b

    override def productPrefix: String = "TProduct"

    def canEqual(t: Any): Boolean = true
  }

  Representable[TProduct.Immutable]
}

object HigherKinded {
  type Id[A] = A

  sealed trait Foo[A[_]]
  case class Bar[A[_]]() extends Foo[A]

  Representable[Bar[Id]]
  Representable[Foo[Id]]

  sealed trait Pipo[A[_]]
  case class Lino() extends Pipo[Id]

  Representable[Pipo[Id]]
}

object Test {
  def main(args: Array[String]): Unit = {
    Syntax
    RepresentableTestsAux
    EnumDefns1
    EnumDefns2
    EnumDefns5
    EnumDefns6
    TestPrefixes1
    PrivateCtorDefns
    PrivateCtor
    Thrift
    HigherKinded
    RepresentableTests.testProductBasics()
    RepresentableTests.testProductVarargs()
    RepresentableTests.testTuples()
    RepresentableTests.testCoproductBasics()
    RepresentableTests.testSingletonCoproducts()
    // RepresentableTests.testOverlappingCoproducts()
    RepresentableTests.testCaseObjects()
    RepresentableTests.testParametrized()
    RepresentableTests.testParametrizedWithVarianceOption()
    // RepresentableTests.testParametrizedWithVarianceList()
    RepresentableTests.testParametrzedSubset()
    RepresentableTests.testParametrizedPermute()
    // RepresentableTests.testAbstractNonCC()
    // RepresentableTests.testNonCCWithCompanion()
    // RepresentableTests.testNonCCLazy()
    // RepresentableTests.testNestedInherited() // Fails at runtime because of #3624
    RepresentableTests.testNonRepresentable()
    RepresentableTests.testNestedCaseObjects()
    // RepresentableTests.testCaseObjectsAndLazy()
    // TestEnum.testEnum0()
    TestEnum.testEnum1()
    TestEnum.testEnum2()
    // TestEnum.testEnum3()
    // TestEnum.testEnum4()
    TestEnum.testEnum5()
    TestEnum.testEnum6()
    // TestEnum.testEnum7()
  }
}
