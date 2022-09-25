class A(val x: Int)
class B(override val x: Int) extends A(x)

class C(x: Int) extends A(x)
case class D(override val x: Int) extends C(x)
