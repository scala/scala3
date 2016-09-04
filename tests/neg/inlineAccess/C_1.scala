package p {
class C {
  protected def f(): Unit = ()

  @dotty.annotation.inline
  def inl() = f() // error (when inlined): not accessible
}
}
