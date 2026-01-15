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

class Cap extends caps.SharedCapability

def test(c: Cap, d: Cap, e: Cap) =

  def f(x: Cap): Unit = if c == x then ()

  def g(x: Cap): Unit = if d == x then ()
  val y = f
  val ys = CONS(y, NIL)
  val zs =
    val z = g
    CONS(z, ys)
  val zsc: LIST[Cap ->{d, y} Unit] = zs
  val z1 = zs.head
  val z1c: Cap ->{y, d} Unit = z1
  val ys1 = zs.tail
  val y1 = ys1.head


  def m1[A, B] =
    (f: A => B) => (xs: LIST[A]) => xs.map(f)

  def m1c: (f: String => Int) -> LIST[String] ->{f} LIST[Int] = m1[String, Int]

  def m2 = [A, B] =>
      (f: A => B) => (xs: LIST[A]) => xs.map(f)

  def m2c: [A, B] -> (f: A => B) -> LIST[A] ->{f} LIST[B] = m2

  def m3 = [A, B] => () =>
      (f: A => B) => (xs: LIST[A]) => xs.map(f)

  def m3c: [A, B] -> () -> (f: A => B) -> LIST[A] ->{f} LIST[B] = m3

  def m4 = [A, B] =>
      (f: A => B) => () => (xs: LIST[A]) => xs.map(f)

  def m4c: [A, B] -> (f: A => B) -> () ->{f} LIST[A] ->{f} LIST[B] = m4

  def eff[A](x: A) = if x == e then x else x

  val eff2 = [A] => (x: A) => if x == e then x else x

  val a0 = identity[Cap ->{d, y} Unit]
  val a0c: (Cap ->{d, y} Unit) ->{d, y} Cap ->{d, y} Unit = a0
  val a1 = zs.map[Cap ->{d, y} Unit](a0)
  val a1c: LIST[Cap ->{d, y} Unit] = a1
  val a2 = zs.map[Cap ->{d, y} Unit](identity[Cap ->{d, y} Unit])
  val a2c: LIST[Cap ->{d, y} Unit] = a2
  val a3 = zs.map(identity[Cap ->{d, y} Unit])
  val a3c: LIST[Cap ->{d, y} Unit] = a3
  val a4 = zs.map(identity)
  val a4c: LIST[Cap ->{d, c} Unit] = a4
  val a5 = map[Cap ->{d, y} Unit, Cap ->{d, y} Unit](identity)(zs)
  val a5c: LIST[Cap ->{d, c} Unit] = a5
  val a6 = m1[Cap ->{d, y} Unit, Cap ->{d, y} Unit](identity)(zs)
  val a6c: LIST[Cap ->{d, c} Unit] = a6

  val b0 = eff[Cap ->{d, y} Unit]
  val b0c: (Cap ->{d, y} Unit) ->{e, d, y} Cap ->{d, y} Unit = b0
  val b1 = zs.map[Cap ->{d, y} Unit](a0)
  val b1c: LIST[Cap ->{d, y} Unit]^{e} = b1
  val b2 = zs.map[Cap ->{d, y} Unit](eff[Cap ->{d, y} Unit])
  val b2c: LIST[Cap ->{d, y} Unit]^{e} = b2
  val b3 = zs.map(eff[Cap ->{d, y} Unit])
  val b3c: LIST[Cap ->{d, y} Unit]^{e} = b3
  val b4 = zs.map(eff)
  val b4c: LIST[Cap ->{d, c} Unit]^{e} = b4
  val b5 = map[Cap ->{d, y} Unit, Cap ->{d, y} Unit](eff)(zs)
  val b5c: LIST[Cap ->{d, c} Unit]^{e} = b5
  val b6 = m1[Cap ->{d, y} Unit, Cap ->{d, y} Unit](eff)(zs)
  val b6c: LIST[Cap ->{d, c} Unit]^{e} = b6

  val c0 = eff2[Cap ->{d, y} Unit]
  val c0c: (Cap ->{d, y} Unit) ->{e, d, y}  Cap ->{d, y} Unit = c0
  val c1 = zs.map[Cap ->{d, y} Unit](a0)
  val c1c: LIST[Cap ->{d, y} Unit]^{e} = c1
  val c2 = zs.map[Cap ->{d, y} Unit](eff2[Cap ->{d, y} Unit])
  val c2c: LIST[Cap ->{d, y} Unit]^{e} = c2
  val c3 = zs.map(eff2[Cap ->{d, y} Unit])
  val c3c: LIST[Cap ->{d, y} Unit]^{e} = c3
