abstract class Parent {
  val f: () => String = () => this.message
  def message: String
}
class Child extends Parent {
  val a = f()
  val b = "hello"           // warn
  def message: String = b
}
