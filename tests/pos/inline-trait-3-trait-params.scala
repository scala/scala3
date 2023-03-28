inline trait A(a: Int):
  def f: Int = a
  def g(b: Int): Int = a + b
end A

class B extends A(4):
  // FIXME: Should not generate second field for A.a (probably in the memoize phase). `B.A$$a` could point to the `a` we already have.
  //        Alternative: remove the field and defs from `A`.

  /*
  <generated> private val a: Int = 4
  <generated> override def f: Int = this.a
  <generated> override def g(x: Int): Int = this.a.+(b)
  */
end B
