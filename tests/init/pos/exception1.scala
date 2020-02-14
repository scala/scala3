// check exception

class Box(var x: Int)

trait Parent {
  def f: Box
}

abstract class Child extends Parent {
  def m: Int = {
    f.x = 10
    10
  }
}
