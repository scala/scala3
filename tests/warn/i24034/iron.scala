
// iron.scala

package iron

import io.circe.*

opaque type IronType[A, C] <: A = A
type :|[A, C] = IronType[A, C]
trait Constraint[A, C]

package constraint:
  object string:
    final class StartWith[V <: String]
    object StartWith:
      inline given [V <: String]: Constraint[String, StartWith[V]] = ???

object circe:
  inline given XXX[A, B](using inline encoder: Encoder[A]): Encoder[A :| B] = ???
  inline given YYY[A, B](using inline encoder: Encoder[A], dummy: scala.util.NotGiven[DummyImplicit]): Encoder[A :| B] = ???
  // inline given [T](using mirror: RefinedTypeOps.Mirror[T], ev: Encoder[mirror.IronType]): Encoder[T] = ???

// trait RefinedTypeOps[A, C, T]:
//   inline given RefinedTypeOps.Mirror[T] = ???
// object RefinedTypeOps:
//   trait Mirror[T]:
//     type BaseType
//     type ConstraintType
//     type IronType = BaseType :| ConstraintType

