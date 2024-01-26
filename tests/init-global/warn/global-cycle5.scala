class X {
  def foo(): Int = 10
}

object A {
  var a: X = new X()
}

object B {
  val b: Int = A.a.foo()
}

class Y extends X {
  override def foo() = C.c
}

object C {
  val c: Int = B.b
}

def main = {
  A.a = new Y(); C
}
