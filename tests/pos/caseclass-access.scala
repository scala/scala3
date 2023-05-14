case class A private (i: Int)
object A {
  def a0 = new A(0)    // constructor is accessible in companion
  def a = A(1).copy(2) // apply and copy are accessible in companion
}

case class B private (i: Int) { // no user-defined companion object, should compile
  def b0 = new B(0)    // constructor is accessible
  def b = B(1).copy(2) // apply and copy are accessible
}

object qualified_private {
  case class A private[qualified_private] (i: Int)
  object A {
    def a0 = new A(0)    // constructor is accessible in companion
    def a = A(1).copy(2) // apply and copy are accessible in companion
  }

  def a0 = new A(0)    // constructor is accessible in qualified_private object
  def a = A(1).copy(2) // apply and copy are accessible in qualified_private object

  case class B private[qualified_private] (i: Int) { // no user-defined companion object, should compile
    def b0 = new B(0)    // constructor is accessible
    def b = B(1).copy(2) // apply and copy are accessible
  }

  def b0 = new B(0)    // constructor is accessible in qualified_private object
  def b = B(1).copy(2) // apply and copy are accessible in qualified_private object
}

case class C protected (i: Int)
class CSub extends C(1) {
  def c = copy(2) // copy is accessible in subclass
}

object qualified_protected {
  case class C protected[qualified_protected] (i: Int)
  class CSub extends C(1) {
    def c = copy(2) // copy is accessible in subclass
  }
  def eta: Int => C = C // can eta-expand C.apply method

  def c0 = new C(0)    // constructor is accessible in qualified_protected object
  def c = C(1).copy(2) // apply and copy are accessible in qualified_protected object
}
