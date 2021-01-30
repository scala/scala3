type T <: foo.a = Int match { // error
  case "" => foo.b // error
}
def foo(x: Int): Unit = ???