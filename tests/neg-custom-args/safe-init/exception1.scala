// check exception

class Box(var x: Int)

trait Parent {
  def f: Box
}

abstract class Child extends Parent {
  @scala.annotation.cold
  def m: Int = {
    f.x = 10
    10
  }
}

object Dummy {
  val a = b      // error
  val b = 1
}
