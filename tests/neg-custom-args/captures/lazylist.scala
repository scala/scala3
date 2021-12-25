package lazylists

abstract class LazyList[+T]:
  this: ({*} LazyList[T]) =>

  def isEmpty: Boolean
  def head: T
  def tail: LazyList[T]

  def map[U](f: T => U): {f, this} LazyList[U] =
    if isEmpty then LazyNil
    else LazyCons(f(head), () => tail.map(f))

class LazyCons[+T](val x: T, val xs: () => {*} LazyList[T]) extends LazyList[T]:
  def isEmpty = false
  def head = x
  def tail = xs() // error: cannot have an inferred type

object LazyNil extends LazyList[Nothing]:
  def isEmpty = true
  def head = ???
  def tail: {*} LazyList[Nothing] = ???  // error overriding

def map[A, B](xs: {*} LazyList[A], f: A => B): {f, xs} LazyList[B] =
  xs.map(f)

class CC
type Cap = {*} CC

def test(cap1: Cap, cap2: Cap, cap3: Cap) =
  def f[T](x: LazyList[T]): LazyList[T] = if cap1 == cap1 then x else LazyNil
  def g(x: Int) = if cap2 == cap2 then x else 0
  def h(x: Int) = if cap3 == cap3 then x else 0
  val ref1 = LazyCons(1, () => f(LazyNil))
  val ref1c: LazyList[Int] = ref1 // error
  val ref2 = map(ref1, g)
  val ref2c: {ref1} LazyList[Int] = ref2 // error
  val ref3 = ref1.map(g)
  val ref3c: {cap2} LazyList[Int] = ref3 // error
  val ref4 = (if cap1 == cap2 then ref1 else ref2).map(h)
  val ref4c: {cap1, ref3, cap3} LazyList[Int] = ref4 // error
