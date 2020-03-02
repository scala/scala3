object test {
  trait B {
    type X; def mkX(i: Int): B#X
  }
  val b: B = new B {
    type X = Int; def mkX(i: Int): X = i  // error
  }
}