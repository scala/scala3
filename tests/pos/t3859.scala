class Test {
  def foo: Unit = bar(Array[AnyRef]()*)
  def bar(values: AnyRef*): Unit = ()
}
