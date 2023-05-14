case class A private (i: Int)
object A
object ATest {
  def a0: A = new A(0) // error: constructor is private
  def a1: A = A(1) // error: apply is private
  def a2: A = a1.copy(2) // error: copy is private
}

case class B private (i: Int) // ok: no user-defined companion object
object BTest {
  def b0: B = new B(0) // error: constructor is private
  def b1: B = B(1) // error: apply is private
  def b2: B = b1.copy(2) // error: copy is private
}

object qualified_private {
  case class C private[qualified_private] (i: Int)
  object C

  case class D private[qualified_private] (i: Int) // ok: no user-defined companion object
}
object QPrivTest {
  import qualified_private.*
  def c0: C = new C(0) // error: constructor is private
  def c1: C = C(1) // error: apply is private
  def c2: C = c1.copy(2) // error: copy is private

  def d0: D = new D(0) // error: constructor is private
  def d1: D = D(1) // error: apply is private
  def d2: D = d1.copy(2) // error: copy is private
}

case class E protected (i: Int)
object ETest {
  def e0: E = new E(0) // error: constructor is protected
  def e1: E = E(1) // error: apply is protected
  def e2: E = e1.copy(2) // error: copy is protected
  def eta: Int => E = E // error: apply is protected
}

object qualified_protected {
  case class F protected[qualified_protected] (i: Int)
}
object QProtTest {
  import qualified_protected.*
  def f0: F = new F(0) // error: constructor is protected
  def f1: F = F(1) // error: apply is protected
  def f2: F = f1.copy(2) // error: copy is protected
  def eta: Int => F = F // error: apply is protected
}
