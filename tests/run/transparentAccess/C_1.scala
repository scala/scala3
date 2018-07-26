package p {
class C {
  protected def f(): Unit = ()

  transparent def inl() = f() // error (when inlined): not accessible
}
}
