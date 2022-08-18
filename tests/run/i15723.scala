class B(val y: Int) {
  println(this.y)
  foo()
  def foo() = println(this.y)
}
class C(override val y: Int) extends B(10)

object Test extends App {
  new C(20)
}