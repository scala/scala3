package p {
class C {
  protected def f(): Unit = ()

  inline def inl() = f() // error (when inlined): not accessible
}
}
