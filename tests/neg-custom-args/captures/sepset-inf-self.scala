import language.experimental.captureChecking
import caps.*

abstract class Reader[T] extends ReadOnly:
  def get: T

abstract class Ref[T](init: T):
  def reader: Reader[T]^{this}
  def set(x: T): Unit

def par[A, B](f: () => A)(g: () ->{cap}!{f} B): (A, B) = ???

def foo(sep x: Ref[Int]^): Unit =  // error
  val op = () => x.set(0)
  par(op)(() => x.set(0))

def bar(op: () => () => Unit, sep x: Ref[Int]^): Unit =
  par(op())(() => x.set(0))  // error
