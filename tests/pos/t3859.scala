class Test {
  def foo: Unit = bar(Array[AnyRef](): _*)
  def bar(values: AnyRef*): Unit = ()
}
