class A {
  class X {
      def outer : A.this.type = A.this
  }
}

class B extends A
class C extends A

object Test {
  def main(args: Array[String]) : Unit = {
    val b0 = new B
    val b1 = b0
    val b2 = new B

    val c0 = new C
    val c1 = c0
    val c2 = new C

    val b0x : A#X = new b0.X

    val pathTypeMatch = b0x match {
      case _ : c2.X => "c2.X"
      case _ : c1.X => "c1.x"
      case _ : c0.X => "c0.X"
      case _ : b2.X => "b2.X"
      case _ : b1.X => "b1.X"
      case _ : b0.X => "b0.X"
      case _        => "ELSE"
    }

    println(pathTypeMatch)

    val projectionTypeMatch = b0x match {
      case _ : C#X => "C#X"
      case _ : B#X => "B#X"
      case _ : A#X => "A#X"
      case _       => "ELSE"
    }

    println(projectionTypeMatch)

    val failingTypeMatch = b0x match {
      case cx : C#X =>
        val c : C = cx.outer
        c
      case _ => "ELSE"
    }

    println(failingTypeMatch)
  }
}