import scala.scalajs.js
import scala.scalajs.js.annotation.*

class A {
  @JSExport("value") // error
  def hello: String = "foo"

  @JSExport("value")
  def world: String = "bar"
}

class B {
  class Box[T](val x: T)

  @JSExport // error
  def ub(x: Box[String]): String = x.x
  @JSExport
  def ub(x: Box[Int]): Int = x.x
}
