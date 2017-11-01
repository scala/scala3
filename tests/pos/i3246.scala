class Test {
  class A {
    type T = Int
    def f(x: T): T = ???
  }
  def a: A = new A
  val f = a.f _
}
