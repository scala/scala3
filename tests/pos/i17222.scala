import scala.deriving.Mirror
import scala.compiletime.*

trait Reader[-In, Out]

trait A:
  type T
  type F[X]
  type Q = F[T]

object Reader:

  given [X]: Reader[A { type Q = X }, X] with {}

  type Map2[Tup1 <: Tuple, Tup2 <: Tuple, F[_, _]] <: Tuple = (Tup1, Tup2) match
    case (h1 *: t1, h2 *: t2) => F[h1, h2] *: Map2[t1, t2, F]
    case (EmptyTuple, EmptyTuple) => EmptyTuple

  inline given productReader[In <: Product, Out <: Product](using mi: Mirror.ProductOf[In])(using mo: Mirror.ProductOf[Out]): Reader[In, Out] =
    summonAll[Map2[mi.MirroredElemTypes, mo.MirroredElemTypes, Reader]]
    ???

object Test:

  trait B[X] extends A:
    type T = X

  trait C extends A:
    type F[X] = X

  val bc = new B[Int] with C

  summon[Reader[(bc.type, bc.type), (Int, Int)]]    // fails
