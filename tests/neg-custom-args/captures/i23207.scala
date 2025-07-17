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
  val c = b.getBox.x
  val _: B^{b} = c // ok
  val _: A = c // error
  c // no error here since we don't propagate expected type into the last expression of a block
    // and the whole block's span overlaps with previous errors

def leak2(io: AnyRef^): A =
  class B extends A:   // error, now we see the error for the whole block since there are no nested errors
    val hide: AnyRef^{io} = io

  val b = new B
  val c = b.getBox.x
  c
