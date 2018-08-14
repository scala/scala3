
object Foo {
  transparent def g(p: (Int => Boolean) & Constant): Boolean = ~{ // error
    if(p(5)) '(true)
    else '(false)
  }
}
