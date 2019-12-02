
object Foo {
  inline def g(inline p: Int => Boolean): Boolean = ${
    if (p(5)) 'true // error
    else 'false
  }
}
