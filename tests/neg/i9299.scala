type F <: F = 1 match { // error
  case _ => foo.foo
}
def foo(a: Int): Unit = ???
