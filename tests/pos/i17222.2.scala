import scala.compiletime.*

trait Reader[-In, Out]

trait A:
  type T
  type F[X]
  type Q = F[T]

object Reader:

  given [X]: Reader[A { type Q = X }, X] with {}

object Test:

  trait B[X] extends A:
    type T = X

  trait C extends A:
    type F[X] = X

  trait D[X] extends B[X] with C

  val d = new D[Int] {}
  val bc = new B[Int] with C

  summonAll[(Reader[d.type, Int], Reader[d.type, Int])] // works
  summonAll[(Reader[bc.type, Int], Reader[bc.type, Int])] // error
  summonInline[Reader[d.type, Int]] // works
  summonInline[Reader[bc.type, Int]] // works??
