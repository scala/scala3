package example

class A {
  type B = Int

  object B {
    inline def f1(x: Int): B = x
  }

  def x1 = B.f1(2)
}