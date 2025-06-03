

class A[T] {
  class X {
      def outer : A.this.type = A.this
  }
}

class B extends A[Int]
class C[T] extends A[T]

object Test {
  def main(args: Array[String]) : Unit = {
    val b0 = new B
    val b0x : A[?]#X = new b0.X

    def test = b0x match {
      case tx : C[Int]#X => // warn
        val c : C[Int] = tx.outer
        c
      case _ =>
        "no match"
    }

    def test2 = b0x match {
      case tx : C[Int]#X @unchecked => // ok
        val c : C[Int] = tx.outer
        c
      case _ =>
        "no match"
    }

  }
}
