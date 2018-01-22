trait Ring[A <: Ring[A]]

object Test {
  def crash[T <: Ring[_ <: T]]: Ring[T] = ??? // error: Type argument T does not conform to upper bound Ring[LazyRef(T)]
}
