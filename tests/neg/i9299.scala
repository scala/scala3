type F <: F = 1 match { // error
  case _ => foo.foo // error // error
}
def foo(a: Int): Unit = ???
