import annotation.capability

@capability class FileSystem

class Logger(using fs: FileSystem):
  def log(s: String): Unit = ???

def test(using fs: FileSystem) =
  val l: {fs} Logger = Logger(using fs)
  l.log("hello world!")
  val xs: {l} LazyList[Int] =
    LazyList.from(1)
      .map { i =>
        l.log(s"computing elem # $i")
        i * i
      }
  xs

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

  def isEmpty = false
  def head = x
  def tail: {this} LazyList[T] = xs()
end LazyCons

extension [A](x: A)
  def #::(xs1: => {*} LazyList[A]): {xs1} LazyList[A] =
    LazyCons(x, () => xs1)

extension [A](xs: {*} LazyList[A])
  def map[B](f: A => B): {xs, f} LazyList[B] =
    if xs.isEmpty then LazyNil
    else f(xs.head) #:: xs.tail.map(f)

object LazyList:
  def from(start: Int): LazyList[Int] =
    start #:: from(start + 1)

