import language.experimental.captureChecking
import caps.*

case class Box[T](x: T)

class A:
  def getBox: Box[this.type] = Box(this)

def leak(io: AnyRef^): A =
  class B extends A:
    val hide: AnyRef^{io} = io

  val b = new B
  val box = b.getBox
  val a: A = box.x // error
  val c = b.getBox.x // now OK
  val _: B^{b} = c
  c