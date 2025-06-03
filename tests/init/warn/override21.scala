abstract class Parent {
  val x = "name"
  lazy val z = bar
  def foo = bar
  def bar: Int
}

class Child extends Parent {
  this.foo
  this.z
  val m = this.x

  val y = "hello"   // warn

  def bar = y.size
}