class Test {
  def to[Col[_]](factory: Factory[Int, Col[Int]]): Col[Int] = ???

  to(Vector)

  type Factory[-A, +C] = scala.collection.generic.CanBuildFrom[Nothing, A, C] // Ideally, this would be an opaque type

  implicit def fromCanBuildFromConversion[X, A, C](x: X)(
    implicit toCanBuildFrom: X => scala.collection.generic.CanBuildFrom[Nothing, A, C]): Factory[A, C] = ???

  implicit def genericCompanionToCBF[A, CC[X] <: scala.collection.GenTraversable[X]](
    fact: scala.collection.generic.GenericCompanion[CC]): scala.collection.generic.CanBuildFrom[Any, A, CC[A]] = ???
}
