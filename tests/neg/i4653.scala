trait T {
  type S
}

class C {
  class D[X](val t: T) {
     def bar: t.S = ???
  }

  def fooD: D = ??? // error
  fooD.bar

  def f(fooV: => D): Any = fooV.bar // error
}
