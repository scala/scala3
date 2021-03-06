class Outer { outer =>
  class Inner extends Outer {
    val x = 5 + outer.n       // error
  }
  val inner = new Inner
  val n = 6               // error
}

class Outer2 { outer =>
  class Inner extends Outer2 {
    val x = 5 + n
  }
  val inner = new Inner
  val n = 6
}
