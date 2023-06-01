import language.experimental.captureChecking
import caps.*

abstract class Reader[T] extends ReadOnly:
  def get: T

abstract class Ref[T](init: T):
  def reader: Reader[T]^{this}
  def set(x: T): Unit

def par[A, B](f: () => A)(sep{f} g: () => B): (A, B) = ???

def test1(a: Ref[Int]^, sep{a} b: Ref[Int]^, sep{a, b} c: Ref[Int]^): Unit = {
  def writeBoth(x: Ref[Int]^)(sep y: Ref[Int]^): Unit =
    par(() => x.set(0))(() => y.set(0))
  writeBoth(a)(b)
  writeBoth(b)(c)
  writeBoth(a)(a)  // error

  def parSwap(x: Ref[Int]^)(sep y: Ref[Int]^): Unit = {
    val rx = x.reader
    val ry = y.reader
    val (x0, y0) = par(() => rx.get)(() => ry.get)
    par(() => x.set(y0))(() => y.set(x0))
  }
  parSwap(a)(b)
  parSwap(a)(a)  // error

  val rb = b.reader
  def foo(sep op: () ->{cap} Unit): Unit = {
    par(op) { () => a.set(rb.get + 1) }
    par(op) { () => c.set(0) }
  }
  foo(() => rb.get)
  foo(() => a.reader.get)  // error
  foo(() => c.set(0))  // error

  def bar(op: () ->{rdr} Unit): Unit = par(op)(op)
  bar(() => rb.get)
  bar(() => a.set(0))  // error
}
