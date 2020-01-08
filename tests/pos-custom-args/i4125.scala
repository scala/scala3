object Test {
  def foo: ((erased x: Int, y: Int) => Int) = (erased x, y) => 1
  def bar: ((given erased x: Int, y: Int) => Int) = (given erased x, y) => 1
}
