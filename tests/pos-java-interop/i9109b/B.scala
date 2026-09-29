class B extends A {
  override def foo[T <: Object & Intf](x: T): Unit = {}
}
