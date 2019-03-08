object Test {

  class C

  def outer(implicit c: C) = {

    def f(c: C) = implicitly[C]   // now ok: shadowing no longer tested
    def g(c: Int) = implicitly[C] // now ok: shadowing no longer tested

    f(new C)
  }

  class C1[X]
  class C2[X]

  def f[T: C1] = {
    def g[U: C2] = {
      implicitly[C1[T]]    // OK: no shadowing for evidence parameters
      implicitly[C2[U]]
    }
  }

  def h[T]: given C1[T] => Unit = {
    def g[U]: given C2[U] => Unit = {
      implicitly[C1[T]]    // OK: no shadowing for evidence parameters
      implicitly[C2[U]]
    }
  }
}
