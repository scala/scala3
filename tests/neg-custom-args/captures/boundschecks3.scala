object test {

  class Tree

  def f[X <: Tree](x: X): Unit = ()

  class C[X <: Tree](x: X)

  val foo: C[Tree^] = ??? // hidden error
  type T = C[Tree^] // hidden error
  val bar: T -> T = ??? // error, since `T` is expanded here. But the msg is not very good.
  val baz: C[Tree^] -> Unit = ??? // hidden error
}
