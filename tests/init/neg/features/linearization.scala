trait TA {
  val x = "world"
}

trait TB {
  def x: String
  val m = "hello" + x
}
class Foo extends TA with TB // OK
class Bar extends TB with TA // error
