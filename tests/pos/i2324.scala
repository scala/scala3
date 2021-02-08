object A {
  def foo: Int = 1
}
object B {
  def foo: Int = 2
}
class C {
  import A.*
  import B.*

  def bar: Int = 4

  def foo: Int = 3

  println(foo)
}
