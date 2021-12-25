class CC
type Cap = {*} CC

trait LazyList[+A]:
  this: ({*} LazyList[A]) =>

  def isEmpty: Boolean
  def head: A
  def tail: {this} LazyList[A]

object LazyNil extends LazyList[Nothing]:
  def isEmpty: Boolean = true
  def head = ???
  def tail = ???

extension [A](xs: {*} LazyList[A])
  def map[B](f: A => B): {xs, f} LazyList[B] =
    final class Mapped extends LazyList[B]:
      this: ({xs, f} Mapped) =>

      def isEmpty = false
      def head: B = f(xs.head)
      def tail: {this} LazyList[B] = xs.tail.map(f)  // OK
      def drop(n: Int): {this} LazyList[B] = ??? : ({xs, f} LazyList[B]) // OK
      def concat(other: {f} LazyList[A]): {this} LazyList[A] = ??? : ({xs, f} LazyList[A]) // error
    new Mapped

