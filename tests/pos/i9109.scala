class A {
  def foo[T <: Serializable](x: T): Unit = {}
  def foo[T <: Cloneable](x: T): Unit = {}
}
class B extends A {
  override def foo[T <: Serializable](x: T): Unit = {}
}
