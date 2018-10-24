abstract class Parent(p: Raw[String]) {
  val x = "name"
  lazy val z = bar
  def foo = bar
  def bar: Int
}

class Child(o: Raw[String]) extends Parent(o) {
  val y = "hello"

  val m = this.x
  this.foo          // error
  this.z            // error

  def bar = y.size
}