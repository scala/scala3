object Test {
  def foo: (erased (x: Int, y: Int) => Int) = erased (x, y) => 1
  def bar: (erased given (x: Int, y: Int) => Int) = erased given (x, y) => 1
}
