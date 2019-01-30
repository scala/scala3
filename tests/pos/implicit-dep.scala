trait HasT {
  type T
}

object Test {


  def foo: given Int => given (g: HasT) => g.T = ???
}
