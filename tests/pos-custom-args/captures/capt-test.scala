abstract class LIST[+T]:
  def isEmpty: Boolean
  def head: T
  def tail: LIST[T]
  def map[U](f: {*} T => U): LIST[U] =
    if isEmpty then NIL
    else CONS(f(head), tail.map(f))

class CONS[+T](x: T, xs: LIST[T]) extends LIST[T]:
  def isEmpty = false
  def head = x
  def tail = xs
object NIL extends LIST[Nothing]:
  def isEmpty = true
  def head = ???
  def tail = ???

def map[A, B](f: {*} A => B)(xs: LIST[A]): LIST[B] =
  xs.map(f)

class C
type Cap = {*} C

def test(c: Cap, d: Cap) =
  def f(x: Cap): Unit = if c == x then ()
  def g(x: Cap): Unit = if d == x then ()
  val y = f
  val ys = CONS(y, NIL)
  val zs =
    val z = g
    CONS(z, ys)
  val zsc: LIST[{d, y} Cap => Unit] = zs

  val a4 = zs.map(identity)
  val a4c: LIST[{d, y} Cap => Unit] = a4
