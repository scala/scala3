object Foo {
  inline val log1 : Boolean = false // error
  inline val log2 = true: Boolean // error
  inline val log3: false  = { println(); false } // error
}
