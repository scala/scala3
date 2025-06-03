trait TA {
  val x = "world" // warn
}

trait TB {
  def x: String
  val m = "hello" + x
}
class Foo extends TA with TB
class Bar extends TB with TA
