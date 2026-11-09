import scala.deriving.Mirror

trait B[T]
case class RB(i: Int) extends B[RB]

// Single F-bounded type parameter
case class FB[T <: B[T]](v: T)

// F-bounded parameter mixed with an ordinary field
case class FB2[T <: B[T]](v: T, n: Int)

// Two F-bounded type parameters
case class FB3[T <: B[T], U <: B[U]](a: T, b: U)

// Standard recursive F-bound on Comparable
case class Ordered[T <: Comparable[T]](v: T)

// F-bounded parameter with a non-Nothing lower bound
case class LU[T >: Null <: B[T]](v: T)

// Covariant F-bounded parameter
trait BC[+T]
case class RBC(i: Int) extends BC[RBC]
case class FBc[+T <: BC[T]](v: T)

// Contravariant F-bounded parameter
trait BN[-T]
case class RBN(i: Int) extends BN[RBN]
case class FBn[-T <: BN[T]](f: T => Int)

// Mutually F-bounded parameters
case class Mut[T <: B[U], U <: B[T]](a: T, b: U)

// F-bound nested inside another type
case class RL(i: Int) extends B[List[RL]]
case class Nest[T <: B[List[T]]](v: T)

// Non-cyclic dependency on another type parameter (not F-bounded, must keep working)
case class Dep[T, U <: Array[T]](a: T, b: U)

@main def Test =
  val m1 = summon[Mirror.ProductOf[FB[RB]]]
  assert(m1.fromProduct(Tuple1(RB(42))) == FB(RB(42)))

  val m2 = summon[Mirror.ProductOf[FB2[RB]]]
  assert(m2.fromProduct((RB(1), 7)) == FB2(RB(1), 7))

  val m3 = summon[Mirror.ProductOf[FB3[RB, RB]]]
  assert(m3.fromProduct((RB(1), RB(2))) == FB3(RB(1), RB(2)))

  val m4 = summon[Mirror.ProductOf[Ordered[String]]]
  assert(m4.fromProduct(Tuple1("hi")) == Ordered("hi"))

  val m5 = summon[Mirror.ProductOf[LU[RB]]]
  assert(m5.fromProduct(Tuple1(RB(3))) == LU(RB(3)))

  val m6 = summon[Mirror.ProductOf[FBc[RBC]]]
  assert(m6.fromProduct(Tuple1(RBC(4))) == FBc(RBC(4)))

  val f: RBN => Int = _.i
  val m7 = summon[Mirror.ProductOf[FBn[RBN]]]
  assert(m7.fromProduct(Tuple1(f)).f(RBN(5)) == 5)

  val m8 = summon[Mirror.ProductOf[Mut[RB, RB]]]
  assert(m8.fromProduct((RB(6), RB(7))) == Mut(RB(6), RB(7)))

  val m9 = summon[Mirror.ProductOf[Nest[RL]]]
  assert(m9.fromProduct(Tuple1(RL(8))) == Nest(RL(8)))

  val m10 = summon[Mirror.ProductOf[Dep[String, Array[String]]]]
  assert(m10.fromProduct(("a", Array("b"))).b.sameElements(Array("b")))

  // The statically reported element type is preserved and usable
  val v1: m1.MirroredElemTypes = Tuple1(RB(99))
  assert(m1.fromProduct(v1).v == RB(99))
