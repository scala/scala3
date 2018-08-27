package p {
class C {
  protected def f(): Unit = ()

  rewrite def inl() = f() // error (when inlined): not accessible
}
}
