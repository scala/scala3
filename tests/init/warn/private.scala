class A(a: Int) {
  a + 3
  def foo() = a * 2
}

class B extends A(3) {
  foo()
  println(a)
  val a = 3     // warn
}

class C extends A(3) {
  foo()
  val a = 3
}