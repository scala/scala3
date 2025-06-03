abstract class Parent {
  val x = "name"
  lazy val z = bar
  def foo = bar
  def bar: Int
}

class Child extends Parent {
  val y = "hello"

  this.foo
  val m = this.x // warn
  this.z

  def bar = m.size + 6
}