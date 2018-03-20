class A(sealed val a: Int) { // error
  sealed lazy val b = 10 // error
}
class B(lazy val a: Int)    // error: parameter may not be lazy
class C(abstract val a: Int)// error
class D {
  def f(sealed a: Int) = 0 // error
  def g(lazy a: Int) = 0 // error
  def g(override a: Int) = 0 // error
  def g(abstract a: Int) = 0 // error
}
sealed erased class E // error
