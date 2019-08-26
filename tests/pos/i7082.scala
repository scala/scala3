object Overloads {

  def foo[V](x: Int = 0, y: Int = 0, z: Int = 0): Nothing = ???

  def foo[V](x: Int, y: Int): Nothing = ???

  def foo[V](x: Int): Nothing = ???

  foo(1)

}