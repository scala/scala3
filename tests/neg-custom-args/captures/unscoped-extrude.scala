import caps.*

class Ref[T](init: T) extends Mutable, Unscoped:
  var x = init
  def get: T = x
  update def put(y: T): Unit = x = y

def Test =
  var x: Ref[String]^ = Ref("x")
  def foo(): Ref[String]^ =
    val r = Ref("y")
    x = r  // should be consumed here
    r      // error



