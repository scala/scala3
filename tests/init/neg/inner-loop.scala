class Outer { outer =>
  class Inner extends Outer {
    val x = 5 + outer.n         // error
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

class Test  {
  class Outer3 { outer =>
    class Inner extends Outer3 {
      val x = 5 + n
    }
    val inner = new Inner
    val n = 6
  }

  val outer = new Outer3
  // Warm objects with inner classes not checked.
  // If we change policy to check more eagerly,
  // the check has to avoid loop here.

  println(outer)            // error

  val m = 10
}