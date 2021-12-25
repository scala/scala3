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
  def map[B](f: A => B): {f} LazyList[B] =
    final class Mapped extends LazyList[B]:  // error
      this: ({xs, f} Mapped) =>

      def isEmpty = false
      def head: B = f(xs.head)
      def tail: {this} LazyList[B] = xs.tail.map(f)
    new Mapped

  def map2[B](f: A => B): {xs} LazyList[B] =
    final class Mapped extends LazyList[B]:  // error
      this: ({xs, f} Mapped) =>

      def isEmpty = false
      def head: B = f(xs.head)
      def tail: {this} LazyList[B] = xs.tail.map(f)
    new Mapped

  def map3[B](f: A => B): {xs} LazyList[B] =
    final class Mapped extends LazyList[B]:
      this: ({xs} Mapped) =>

      def isEmpty = false
      def head: B = f(xs.head)
      def tail: {this} LazyList[B] = xs.tail.map(f) // error
    new Mapped

  def map4[B](f: A => B): {xs} LazyList[B] =
    final class Mapped extends LazyList[B]:
      this: ({xs, f} Mapped) =>

      def isEmpty = false
      def head: B = f(xs.head)
      def tail: {xs, f} LazyList[B] = xs.tail.map(f) // error
    new Mapped

  def map5[B](f: A => B): LazyList[B] =
    class Mapped extends LazyList[B]:
      this: ({xs, f} Mapped) =>

      def isEmpty = false
      def head: B = f(xs.head)
      def tail: {this} LazyList[B] = xs.tail.map(f) // error
    class Mapped2 extends Mapped:
      this: Mapped =>
    new Mapped2


