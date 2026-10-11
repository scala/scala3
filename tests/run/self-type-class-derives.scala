//> using options -language:experimental.modularity

trait Named:
  type Self
  def name: String

object Named:
  def derived[X]: Named { type Self = X } = new Named:
    type Self = X
    def name = "derived"

given intNamed: (Int is Named):
  def name = "Int"

// Arity 0: no evidence needed.
case class Mono() derives Named

// Arity >= 1: one evidence instance per ADT type parameter, mirroring
// deriveSingleParameter's case (b) for ordinary kind-* type classes.
case class Poly[A](a: A) derives Named
case class Poly2[A, B](a: A, b: B) derives Named

@main def Test(): Unit =
  assert(summon[Mono is Named].name == "derived")
  assert(summon[Named { type Self = Int }].name == "Int")
  assert(summon[Poly[Int] is Named].name == "derived")
  assert(summon[Named { type Self = Poly2[Int, Int] }].name == "derived")
