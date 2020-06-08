class B extends A {
  override def foo[T <: Serializable](x: T): Unit = {}
}
