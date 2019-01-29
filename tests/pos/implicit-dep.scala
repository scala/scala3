trait HasT {
  type T
}

object Test {


  def foo: Int |=> (g: HasT) |=> g.T = ???
}
