class A(sealed val a: Int) // error
class B(lazy val a: Int) // error
class C(abstract val a: Int) // error
class D {
  def f(sealed a: Int) = 0 // error
  def g(lazy a: Int) = 0 // error
  def g(override a: Int) = 0 // error
  def g(abstract a: Int) = 0 // error
}
