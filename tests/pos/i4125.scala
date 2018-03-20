object Test {
  def foo: (erased (x: Int, y: Int) => Int) = erased (x, y) => 1
  def bar: (erased implicit (x: Int, y: Int) => Int) = erased implicit (x, y) => 1
}
