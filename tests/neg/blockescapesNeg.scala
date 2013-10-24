object blockescapesNeg {
  def m0 = { object Foo { class Bar } ; new Foo.Bar }
  class A[T]
  def m1 = { val x = 1; new A[x.type]}
}