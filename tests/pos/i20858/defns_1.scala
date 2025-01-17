import scala.compiletime.*
import scala.deriving.*

sealed trait ZIO[-R, +E, +A]
sealed abstract class ZLayer[-RIn, +E, +ROut]
object ZLayer:
  def apply[RIn, E, ROut](zio: => ZIO[RIn, E, ROut]): ZLayer[RIn, E, ROut] = ???
type URIO[-R, +A] = ZIO[R, Nothing, A]
type IAnyType[T <: Tuple] = Tuple.Fold[T, Any, [x, y] =>> x & y]
type UAnyType[T <: Tuple] = Tuple.Fold[T, Any, [x, y] =>> x | y]


trait AutoLayer[A]:
  def zlayer(using
             p: Mirror.ProductOf[A]
            ): ZLayer[IAnyType[p.MirroredElemTypes], Nothing, A]

object AutoLayer:
  inline given derived: [A] => (p: Mirror.ProductOf[A]) => AutoLayer[A] = {
    val a: ZIO[IAnyType[p.MirroredElemTypes], Nothing, A] = ???
    new AutoLayer[A]:
      override def zlayer(using
                          pp: Mirror.ProductOf[A]
                         ): ZLayer[IAnyType[pp.MirroredElemTypes], Nothing, A] = ZLayer {
        a.asInstanceOf[ZIO[IAnyType[pp.MirroredElemTypes], Nothing, A]]
      }
  }