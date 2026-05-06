object test {

  class Tree

  def f[X <: Tree](x: X): Unit = ()

  class C[X <: Tree](x: X)

  def foo(t: Tree^) =
    f(t) // error
    f[Tree^](t) // error
    f[Tree](t)  // error
    val c1 = C(t) // error
    val c2 = C[Tree^](t) // error // error
    val c3 = C[Tree](t)  // error

    val foo: C[Tree^] = ???
}
