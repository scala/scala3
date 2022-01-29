import language.experimental.saferExceptions
import annotation.unchecked.uncheckedVariance

trait LazyList[+A]:
  this: {*} LazyList[A] =>

  def isEmpty: Boolean
  def head: A
  def tail: {this} LazyList[A]

object LazyNil extends LazyList[Nothing]:
  def isEmpty: Boolean = true
  def head = ???
  def tail = ???

final class LazyCons[+T](val x: T, val xs: () => {*} LazyList[T]) extends LazyList[T]:
  this: {*} LazyList[T] =>

  var forced = false
  var cache: {this} LazyList[T @uncheckedVariance] = compiletime.uninitialized

  private def force =
    if !forced then
      cache = xs()
      forced = true
    cache

  def isEmpty = false
  def head = x
  def tail: {this} LazyList[T] = force

extension [A](xs: {*} LazyList[A])
  def map[B](f: A => B): {xs, f} LazyList[B] =
    if xs.isEmpty then LazyNil
    else LazyCons(f(xs.head), () => xs.tail.map(f))

class Ex1 extends Exception
class Ex2 extends Exception

def test(using cap1: CanThrow[Ex1], cap2: CanThrow[Ex2]) =
  val xs = LazyCons(1, () => LazyNil)

  def f(x: Int): Int throws Ex1 =
    if x < 0 then throw Ex1()
    x * x

  val res = xs.map(f)
  res: {cap1} LazyList[Int]

