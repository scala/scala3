import scala.compiletime.*

trait Reader[-In, Out]

trait A:
  type T
  type F[X]
  type Q = F[T]

given [X]: Reader[A { type Q = X }, X] with {}

case class Box[T](x: T)

inline def summonOne[T]: T =
  summonInline[T]
end summonOne

@main def main =
  trait B[X] extends A:
    type T = X
  trait C extends A:
    type F[X] = X

  val bc = new B[Int] with C
  summonInline[Reader[bc.type, Int]] // (I) Works
  summonOne[Reader[bc.type, Int]] // (II) Errors
