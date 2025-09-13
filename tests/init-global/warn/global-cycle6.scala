object A { // warn
  val n: Int = B.m
  class Inner {
    println(n) // warn
  }
}

object B {
  val a = new A.Inner
  val m: Int = 10
}

object O {
  object A {
    val n: Int = B.m
    class Inner {
      val x: Int = 4
    }
  }

  object B {
    val a = new A.Inner
    val m: Int = 10
  }
}