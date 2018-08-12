
object Foo {
  rewrite def g(transparent p: Int => Boolean): Boolean = ~{ // error
    if(p(5)) '(true)
    else '(false)
  }
}
