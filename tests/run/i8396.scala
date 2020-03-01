trait Show[A]:
  def show(a: A): String = a.toString

object Prefix:
  type AbstractType
  type UpperBoundedType <: String
  type FullyBoundedType >: String <: String

  given A as Show[AbstractType]
  given B as Show[UpperBoundedType]
  given C as Show[FullyBoundedType]

@main def Test =
  summon[Show[Prefix.AbstractType]]
  summon[Show[Prefix.UpperBoundedType]]
  summon[Show[Prefix.FullyBoundedType]]
