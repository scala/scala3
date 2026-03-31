package lazylists

abstract class LazyList[+T]:
  this: LazyList[T]^ =>

  def isEmpty: Boolean
  def head: T
  def tail: LazyList[T]

  def map[U](f: T => U): LazyList[U]^{f, this} =
    if isEmpty then LazyNil
    else LazyCons(f(head), () => tail.map(f))

class LazyCons[+T](val x: T, val xs: () => LazyList[T]^) extends LazyList[T]:
  def isEmpty = false
  def head = x
  def tail = xs() // error

object LazyNil extends LazyList[Nothing]:
  def isEmpty = true
  def head = ???
  def tail: LazyList[Nothing]^ = ???  // error overriding

def map[A, B](xs: LazyList[A]^, f: A => B): LazyList[B]^{f, xs} =
  xs.map(f)

class CC
type Cap = CC^

val cap1: Cap = CC()
val cap2: Cap = CC()
val cap3: Cap = CC()

def test() =
  def f[T](x: LazyList[T]): LazyList[T] = if cap1 == cap1 then x else LazyNil
  def g(x: Int) = if cap2 == cap2 then x else 0
  def h(x: Int) = if cap3 == cap3 then x else 0
  val ref1 = LazyCons(1, () => f(LazyNil))
  val ref1c: LazyList[Int] = ref1 // error
  val ref2 = map(ref1, g)
  val ref2c: LazyList[Int]^{ref1} = ref2 // error
  val ref3 = ref1.map(g)
  val ref3c: LazyList[Int]^{cap2} = ref3 // error
  val ref4 = (if cap1 == cap2 then ref1 else ref2).map(h)
  val ref4c: LazyList[Int]^{cap1, ref3} = ref4 // error
