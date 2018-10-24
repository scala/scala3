abstract class Parent(p: Cold[String]) {
  val x = "name"
  lazy val z = bar
  def foo = bar
  def bar: Int
}

class Child(o: Cold[String]) extends Parent(o) {
  val y = "hello"

  val m = this.x
  this.foo          // error
  this.z            // error

  def bar = y.size
}