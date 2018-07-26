
object Foo {
  transparent def g(transparent p: Int => Boolean): Boolean = ~{
    if(p(5)) '(true) // error
    else '(false)
  }
}
