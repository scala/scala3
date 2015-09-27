object blockescapesNeg {
  def m0 = { object Foo { class Bar { val field = 2 }} ; new Foo.Bar }
  m0.field                                            // error
  class A[T]
  def m1 = { val x = 1; new A[x.type]}
}
