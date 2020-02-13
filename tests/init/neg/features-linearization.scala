trait TA {
  val x = "world" // error
}

trait TB {
  def x: String
  val m = "hello" + x
}
class Foo extends TA with TB
class Bar extends TB with TA
