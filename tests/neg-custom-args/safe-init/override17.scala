class A { self : B =>
  val y = x  // error
}

class B {
  val x = 10
}