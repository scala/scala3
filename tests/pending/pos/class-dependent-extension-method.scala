class C(val a: String) extends AnyVal {
  def foo[U <: a.type]: Unit = foo[U]
}
