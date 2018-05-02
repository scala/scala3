class Parent {
  def foo(): Int = 5
}

class Child extends Parent {
  val a = 4

  def g() = foo()  // error
  g()  // error

  val b = 10
  g()  // error, as `Child` is not final
}