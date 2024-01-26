object O {
  case class Data(x: Int) extends (Int => Int) {
    def apply(x: Int) = x * x
  }
  val d = Data(3)
}

object A {
  val n: Int = B.m
}

object B {
  val m: Int = A.n
}

