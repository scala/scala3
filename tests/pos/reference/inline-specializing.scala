package inlinespecializing

object Test{
  class A
  class B extends A {
  def meth() = true
  }

  inline def choose(b: Boolean) <: A = {
  if (b) new A()
  else new B()
  }

  val obj1 = choose(true)  // static type is A
  val obj2 = choose(false) // static type is B

  // obj1.meth() // compile-time error
  obj2.meth() // OK
}