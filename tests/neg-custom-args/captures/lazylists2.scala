class CC
type Cap = CC^

trait LazyList[+A]:
  this: LazyList[A]^ =>

  def isEmpty: Boolean
  def head: A
  def tail: LazyList[A]^{this}

object LazyNil extends LazyList[Nothing]:
  def isEmpty: Boolean = true
  def head = ???
  def tail = ???

extension [A](xs: LazyList[A]^)
  def map[B](f: A => B): LazyList[B]^{f} =
    final class Mapped extends LazyList[B]:
      this: (Mapped^{xs, f}) =>

      def isEmpty = false
      def head: B = f(xs.head)
      def tail: LazyList[B]^{this} = xs.tail.map(f)
    new Mapped  // error

  def map2[B](f: A => B): LazyList[B]^{xs} =
    final class Mapped extends LazyList[B]:
      this: Mapped^{xs, f} =>

      def isEmpty = false
      def head: B = f(xs.head)
      def tail: LazyList[B]^{this} = xs.tail.map(f)
    new Mapped  // error

  def map3[B](f: A => B): LazyList[B]^{xs} =
    final class Mapped extends LazyList[B]:
      this: Mapped^{xs} =>

      def isEmpty = false
      def head: B = f(xs.head)  // error
      def tail: LazyList[B]^{this}= xs.tail.map(f) // error
    new Mapped

  def map4[B](f: A => B): LazyList[B]^{xs} =
    final class Mapped extends LazyList[B]:
      this: (Mapped^{xs, f}) =>

      def isEmpty = false
      def head: B = f(xs.head)
      def tail: LazyList[B]^{xs, f} = xs.tail.map(f)
    new Mapped  // error

  def map5[B](f: A => B): LazyList[B] =
    class Mapped extends LazyList[B]:
      this: (Mapped^{xs, f}) =>

      def isEmpty = false
      def head: B = f(xs.head)
      def tail: LazyList[B]^{this} = xs.tail.map(f)
    class Mapped2 extends Mapped:  // error
      this: Mapped =>
    new Mapped2 // error


