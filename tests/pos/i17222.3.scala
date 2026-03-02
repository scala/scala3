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

  case class Box[T](value: T)

  /** compiletime.summonAll, but with one case */
  inline def summonOne[T <: Box[?]]: T =
    val res =
      inline erasedValue[T] match
        case _: Box[t] => summonInline[t]
      end match
    Box(res).asInstanceOf[T]
  end summonOne

  summonOne[Box[Reader[d.type, Int]]] // works
  summonOne[Box[Reader[bc.type, Int]]] // errors
