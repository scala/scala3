abstract class LIST[+T]:
  def isEmpty: Boolean
  def head: T
  def tail: LIST[T]
  def map[U](f: T => U): LIST[U] =
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

def map[A, B](f: A => B)(xs: LIST[A]): LIST[B] =
  xs.map(f)

@annotation.capability class Cap

def test(c: Cap, d: Cap, e: Cap) =
  def f(x: Cap): Unit = if c == x then ()
  def g(x: Cap): Unit = if d == x then ()
  val y = f
  val ys = CONS(y, NIL)
  val zs =
    val z = g
    CONS(z, ys)
  val zsc: LIST[{d, y} Cap -> Unit] = zs
  val z1 = zs.head
  val z1c: {y, d} Cap -> Unit = z1
  val ys1 = zs.tail
  val y1 = ys1.head


  def m1[A, B] =
    (f: A => B) => (xs: LIST[A]) => xs.map(f)

  def m1c: (f: String => Int) -> {f} LIST[String] -> LIST[Int] = m1[String, Int]

  def m2 = [A, B] =>
      (f: A => B) => (xs: LIST[A]) => xs.map(f)

  def m2c: [A, B] -> (f: A => B) -> {f} LIST[A] -> LIST[B] = m2

  def eff[A](x: A) = if x == e then x else x

  val eff2 = [A] => (x: A) => if x == e then x else x

  val a0 = identity[{d, y} Cap -> Unit]
  val a0c: ({d, y} Cap -> Unit) -> {d, y} Cap -> Unit = a0
  val a1 = zs.map[{d, y} Cap -> Unit](a0)
  val a1c: LIST[{d, y} Cap -> Unit] = a1
  val a2 = zs.map[{d, y} Cap -> Unit](identity[{d, y} Cap -> Unit])
  val a2c: LIST[{d, y} Cap -> Unit] = a2
  val a3 = zs.map(identity[{d, y} Cap -> Unit])
  val a3c: LIST[{d, y} Cap -> Unit] = a3
  val a4 = zs.map(identity)
  val a4c: LIST[{d, c} Cap -> Unit] = a4
  val a5 = map[{d, y} Cap -> Unit, {d, y} Cap -> Unit](identity)(zs)
  val a5c: LIST[{d, c} Cap -> Unit] = a5
  val a6 = m1[{d, y} Cap -> Unit, {d, y} Cap -> Unit](identity)(zs)
  val a6c: LIST[{d, c} Cap -> Unit] = a6

  val b0 = eff[{d, y} Cap -> Unit]
  val b0c: {e} ({d, y} Cap -> Unit) -> {d, y} Cap -> Unit = b0
  val b1 = zs.map[{d, y} Cap -> Unit](a0)
  val b1c: {e} LIST[{d, y} Cap -> Unit] = b1
  val b2 = zs.map[{d, y} Cap -> Unit](eff[{d, y} Cap -> Unit])
  val b2c: {e} LIST[{d, y} Cap -> Unit] = b2
  val b3 = zs.map(eff[{d, y} Cap -> Unit])
  val b3c: {e} LIST[{d, y} Cap -> Unit] = b3
  val b4 = zs.map(eff)
  val b4c: {e} LIST[{d, c} Cap -> Unit] = b4
  val b5 = map[{d, y} Cap -> Unit, {d, y} Cap -> Unit](eff)(zs)
  val b5c: {e} LIST[{d, c} Cap -> Unit] = b5
  val b6 = m1[{d, y} Cap -> Unit, {d, y} Cap -> Unit](eff)(zs)
  val b6c: {e} LIST[{d, c} Cap -> Unit] = b6

  val c0 = eff2[{d, y} Cap -> Unit]
  val c0c: {e} ({d, y} Cap -> Unit) -> {d, y} Cap -> Unit = c0
  val c1 = zs.map[{d, y} Cap -> Unit](a0)
  val c1c: {e} LIST[{d, y} Cap -> Unit] = c1
  val c2 = zs.map[{d, y} Cap -> Unit](eff2[{d, y} Cap -> Unit])
  val c2c: {e} LIST[{d, y} Cap -> Unit] = c2
  val c3 = zs.map(eff2[{d, y} Cap -> Unit])
  val c3c: {e} LIST[{d, y} Cap -> Unit] = c3

