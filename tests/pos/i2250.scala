class T {
  def foo(any: AnyRef): Unit = {
    any.asInstanceOf[Array[_]].iterator
  }
}