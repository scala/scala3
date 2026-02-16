trait A {
  class T()
}
trait B {
  this: A =>
  def f(a: Int = 0): Any
}
trait C extends B {
  this: A =>
  def f(t: T): Any
}