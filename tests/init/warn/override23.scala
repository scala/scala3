abstract class Parent(p: String) {
  val x = "name"
  lazy val z = bar
  def foo = bar
  def bar: Int
}

class Child(o: String) extends Parent(o) {
  val m = this.x
  this.foo
  this.z

  val y = "hello"    // warn

  def bar = y.size
}