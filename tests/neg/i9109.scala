class A {
  def foo[T <: Cloneable](x: T): Unit = {}
}
class B extends A {
  override def foo[T <: Serializable](x: T): Unit = {} // error: method foo has a different signature than the overridden declaration
}
