type F <: F = 1 match { // error: Recursion limit exceeded.
  case _ => foo.foo // error: Recursion limit exceeded.
}
def foo(a: Int): Unit = ???
