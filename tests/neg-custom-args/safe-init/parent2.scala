abstract class Parent {
  val x = "name"
  lazy val z = bar
  def foo = bar
  def bar: Int
}

class Child extends Parent {
  val y = "hello"

  this.foo          // error
  val m = this.x
  this.z            // error

  def bar = m.size + 6
}