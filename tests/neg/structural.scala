object Test3 {
  import scala.reflect.Selectable.reflectiveSelectable
  def g(x: { type T ; def t: T ; def f(a: T): Boolean }) = x.f(x.t) // error: no ClassTag for x.T
  g(new { type T = Int; def t = 4; def f(a:T) = true })
  g(new { type T = Any; def t = 4; def f(a:T) = true })
  val y: { type T = Int; def t = 4; def f(a:T) = true }  // error: illegal refinement // error: illegal refinement
    = new { type T = Int; def t = 4; def f(a:T) = true }

  def h(x: { def f[T](a: T): Int }) = x.f[Int](4) // error: polymorphic refinement method ... no longer allowed

  type A = { def foo(x: Int): Unit; def foo(x: String): Unit } // error: overloaded definition // error: overloaded definition
  type B = { val foo: Int; def foo: Int } // error: duplicate foo
}
