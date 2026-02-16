sealed trait CaseSet:
  type EnumType

object CaseSet {
  def caseOf[A, Z >: A]: Cons[A, Empty[Z], Z] = ???
  type Aux[EnumType0] = CaseSet { type EnumType = EnumType0 }

  final class Empty[Z] extends CaseSet:
    type EnumType = Z

  final class Cons[A, +T <: CaseSet.Aux[Z], Z](head: A, tail: T) extends CaseSet:
    type EnumType = Z
    def ++[That](that: That)(implicit append: Append[Z, Cons[A, T, Z], That]): append.Out = ???
}

sealed trait Append[EnumType, -Left, -Right]:
  type Out <: CaseSet.Aux[EnumType]

object Append:
  type WithOut[EnumType, Left, Right, Out0] = Append[EnumType, Left, Right] { type Out = Out0 }

  implicit def AppendCons[A, Z, T <: CaseSet.Aux[Z], That <: CaseSet.Aux[Z]](implicit
      append: Append[Z, T, That]
  ): Append.WithOut[Z, CaseSet.Cons[A, T, Z], That, CaseSet.Cons[A, append.Out, Z]] = ???

  implicit def AppendEmptyLeft[T <: CaseSet.Aux[Z], Z]: Append.WithOut[Z, CaseSet.Empty[Z], T, T] =
    ???

object Test:
  type UnionValue = Int | Boolean | String
  val _ = CaseSet.caseOf[Int, UnionValue] ++
      CaseSet.caseOf[Boolean, UnionValue] ++
      CaseSet.caseOf[String, UnionValue]