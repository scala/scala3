object A {
  inline def foo(a: Any): Unit = a match {
    case _: Int =>
    case _ =>
  }
}
