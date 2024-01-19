object test {

  class Tree

  def f[X <: Tree](x: X): Unit = ()

  class C[X <: Tree](x: X)

  val foo: C[Tree^] = ??? // error
  type T = C[Tree^] // error
  val bar: T -> T = ???
  val baz: C[Tree^] -> Unit = ??? // error
}
