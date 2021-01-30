class A
class B extends A

def test: Unit =
  println(Invalid[A, B]) // compiles as expected
  println(Invalid[B, A]) // error
