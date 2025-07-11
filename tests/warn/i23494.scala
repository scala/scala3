//> using options -Wunused:implicits

import scala.deriving.Mirror

abstract class EnumerationValues[A]:
  type Out

object EnumerationValues:
  type Aux[A, B] = EnumerationValues[A] { type Out = B }

  def apply[A, B](): EnumerationValues.Aux[A, B] = new EnumerationValues[A]:
    override type Out = B

  given sum[A, B <: Tuple](using mirror: Mirror.SumOf[A] { type MirroredElemTypes = B }): EnumerationValues.Aux[A, A] =
    EnumerationValues[A, A]()
