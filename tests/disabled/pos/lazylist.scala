package lazylists

abstract class LazyList[+T]:
  this: ({*} LazyList[T]) =>

  def isEmpty: Boolean
  def head: T
  def tail: LazyList[T]

  def map[U](f: {*} T => U): {f, this} LazyList[U] =
    if isEmpty then LazyNil
    else LazyCons(f(head), () => tail.map(f))

  def concat[U >: T](that: {*} LazyList[U]): {this, that} LazyList[U]

//  def flatMap[U](f: {*} T => LazyList[U]): {f, this} LazyList[U]

class LazyCons[+T](val x: T, val xs: {*} () => {*} LazyList[T]) extends LazyList[T]:
  def isEmpty = false
  def head = x
  def tail: {*} LazyList[T] = xs()
  def concat[U >: T](that: {*} LazyList[U]): {this, that} LazyList[U] =
    LazyCons(x, () => xs().concat(that))
//  def flatMap[U](f: {*} T => LazyList[U]): {f, this} LazyList[U] =
//    f(x).concat(xs().flatMap(f))

object LazyNil extends LazyList[Nothing]:
  def isEmpty = true
  def head = ???
  def tail = ???
  def concat[U](that: {*} LazyList[U]): {that} LazyList[U] = that
//  def flatMap[U](f: {*} Nothing => LazyList[U]): LazyList[U] = LazyNil

def map[A, B](xs: {*} LazyList[A], f: {*} A => B): {f, xs} LazyList[B] =
  xs.map(f)

class CC
type Cap = {*} CC

def test(cap1: Cap, cap2: Cap, cap3: Cap) =
  def f[T](x: LazyList[T]): LazyList[T] = if cap1 == cap1 then x else LazyNil
  def g(x: Int) = if cap2 == cap2 then x else 0
  def h(x: Int) = if cap3 == cap3 then x else 0
  val ref1 = LazyCons(1, () => f(LazyNil))
  val ref1c: {cap1} LazyList[Int] = ref1
  val ref2 = map(ref1, g)
  val ref2c: {cap2, ref1} LazyList[Int] = ref2
  val ref3 = ref1.map(g)
  val ref3c: {cap2, ref1} LazyList[Int] = ref3
  val ref4 = (if cap1 == cap2 then ref1 else ref2).map(h)
  val ref4c: {cap1, cap2, cap3} LazyList[Int] = ref4