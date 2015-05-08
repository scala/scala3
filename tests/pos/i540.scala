trait Foo extends Any

object Univ {
  def univ[T <: Foo](x: Array[T]) = {}
  def univ2(x: Array[_ <: Foo]) = {}
}
