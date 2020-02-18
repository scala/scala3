class A(a: Int) {
  val _ = a + 3
  def foo() = a * 2
}

class B extends A(3) {
  foo()
  println(a)
  val a = 3     // error
}

class C extends A(3) {
  foo()
  val a = 3
}