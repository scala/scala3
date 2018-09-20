class A(a: Int) {
  trait X {
    def f: Int
    val x = a + f      // NPE: the outer for `Y` is not yet set
  }

  trait Y {
    val y = a
    def f: Int = A.this.a      // NPE: the outer for `Y` is not yet set
  }

  class Z(o: A) extends m.Y {  // error
    val m = o
  }

  class B extends Z(new A(4))
}


object Test {
  def main(args: Array[String]): Unit = {
    val a = new A(3)
    new a.B
  }
}
