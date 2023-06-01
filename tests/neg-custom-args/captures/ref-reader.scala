import language.experimental.captureChecking
import caps.*

abstract class Reader[T] extends ReadOnly:
  def get: T

abstract class Ref[T](init: T):
  def reader: Reader[T]^{this}
  def set(x: T): Unit

type Proc = () -> Unit
def test1(a: Ref[Int]^, sep{a} b: Ref[Boolean]^, sep{a, b} c: Ref[String]^): Unit = {
  val ra = a.reader
  val rb = b.reader
  val rc = c.reader

  val f1: Proc^ !{a} = () => b.set(rc.get.isEmpty)
  val f2: Proc^ !{b} = () => b.set(rc.get.isEmpty)  // error
  val f3: Proc^ !{c} = () => b.set(rc.get.isEmpty)  // error
  val f4: Proc^ !{rc} = () => b.set(rc.get.isEmpty)
}

def par(f: Proc^)(sep{f} g: Proc^): Unit = ???
def test2(a: Ref[Int]^, sep{a} b: Ref[Boolean]^, sep{a, b} c: Ref[String]^): Unit = {
  val ra = a.reader
  val rb = b.reader
  val rc = c.reader

  par(() => a.set(0))(() => b.set(false))
  par(() => a.set(0))(() => b.set(rc.get.isEmpty))
  par(() => b.set(ra.get > 0))(() => c.set(ra.get.toString))
  par(() => b.set(ra.get > 0))(() => a.set(rc.get.toInt))  // error
  par(() => a.set(0))(() => ra.get)  // error
  par(() => ra.get)(() => ra.get)
}

def test3(a: Ref[Int]^, b: Ref[Int]^): Unit = {
  val ra = a.reader
  val rb = b.reader

  par(() => ra.get)(() => rb.get)
  par(() => ra.get)(() => ra.get)
  par(() => a.set(0))(() => b.set(0))  // error
}
