trait HasT {
  type T
}

object Test {


  def foo: implicit Int => implicit (g: HasT) => g.T = ???
}
