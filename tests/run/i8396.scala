trait Show[A]:
  def show(a: A): String = a.toString

object Prefix:
  type AbstractType
  type UpperBoundedType <: String
  type FullyBoundedType >: String <: String

  given A: Show[AbstractType]()
  given B: Show[UpperBoundedType]()
  given C: Show[FullyBoundedType]()

@main def Test =
  summon[Show[Prefix.AbstractType]]
  summon[Show[Prefix.UpperBoundedType]]
  summon[Show[Prefix.FullyBoundedType]]
