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

@main def Test =
  val m1 = summon[Mirror.ProductOf[FB[RB]]]
  assert(m1.fromProduct(Tuple1(RB(42))) == FB(RB(42)))

  val m2 = summon[Mirror.ProductOf[FB2[RB]]]
  assert(m2.fromProduct((RB(1), 7)) == FB2(RB(1), 7))

  val m3 = summon[Mirror.ProductOf[FB3[RB, RB]]]
  assert(m3.fromProduct((RB(1), RB(2))) == FB3(RB(1), RB(2)))

  val m4 = summon[Mirror.ProductOf[Ordered[String]]]
  assert(m4.fromProduct(Tuple1("hi")) == Ordered("hi"))

  // The statically reported element type is preserved and usable
  val v1: m1.MirroredElemTypes = Tuple1(RB(99))
  assert(m1.fromProduct(v1).v == RB(99))
