class A(a: Int) {
  abstract class X {
    def f: Int
    val x = a + f      // NPE: the outer for `Y` is not yet set
  }

  trait Y {
    val y = a
    def f: Int = A.this.a      // NPE: the outer for `Y` is not yet set
  }

  trait Z(val o: A) extends o.Y {  // error
    val z = a
  }

  class B extends X with Z(new A(4))
}


object Test {
  def main(args: Array[String]): Unit = {
    val a = new A(3)
    new a.B
  }
}
