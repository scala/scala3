abstract class Parent {
  val x = "name"
  lazy val z = bar
  def foo = bar
  def bar: Int
}

class Child extends Parent {
  this.foo          // error
  this.z            // error
  val m = this.x

  val y = "hello"

  def bar = y.size
}