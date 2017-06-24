case class A()
case class B() extends A() // error
class C extends A()
case class D() extends C // error
