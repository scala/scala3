object A {
  inline def foo(a: Any): Unit = a match {
    case _: Int => // error
    case _ =>
  }
}
