object InfiniteSubtypingLoopPossibility {
  trait A[X]
  trait B extends A[B]
  trait Min[+S <: B with A[S]]

  def c: Any = ???
  c match {
    case pc: Min[_] =>
  }
}
