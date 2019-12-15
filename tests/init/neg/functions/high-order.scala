abstract class Parent {
  val f: () => String = () => this.message
  def message: String
}
class Child extends Parent {
  val a = f()
  val b = "hello"           // error
  def message: String = b
}
