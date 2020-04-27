class B(y: Int) extends A(new C(y){})

class A(c: C)

abstract class C(y: Int) {
  def x: Int = y
}
