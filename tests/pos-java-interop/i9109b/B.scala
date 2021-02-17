class B extends A {
  override def foo[T <: Object with Intf](x: T): Unit = {}
}
