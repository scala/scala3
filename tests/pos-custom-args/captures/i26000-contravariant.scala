package i26000contravariant

import language.experimental.captureChecking

class File
class Box[+A](val value: A)
trait Sink[-A] { def put(a: A): Unit }

object Lib:
  def both[A](a: A): (Sink[A], Box[A]) = (new Sink[A] { def put(x: A) = () }, Box(a))
  def pairWith[A](a: A): (A, A => Unit) = (a, (b: A) => ())

// A `^` in a declared parameter type below a contravariant position stays global,
// as in the same explicit type
object Use:
  val sb = Lib.both((x: File^) => ())
  def local(): Unit =
    val l = Lib.both((x: File^) => ())
    l._1.put((x: File^) => ())
  def pair(): Unit =
    val pw = Lib.pairWith((x: File^) => ())
    val g: (x: File^) -> Unit = pw._1
