class A {
  def foo1[T <: Serializable](x: Array[T]): Unit = {}
  def foo2[T <: Object & Serializable](x: Array[T]): Unit = {}
}
