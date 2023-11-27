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
  def map[B](f: A => B): LazyList[B]^{xs, f} =
    final class Mapped extends LazyList[B]:
      this: (Mapped^{xs, f}) =>

      def isEmpty = false
      def head: B = f(xs.head)
      def tail: LazyList[B]^{this} = xs.tail.map(f)  // OK
      def drop(n: Int): LazyList[B]^{this} = ??? : (LazyList[B]^{xs, f}) // OK
      def concat(other: LazyList[A]^{f}): LazyList[A]^{this, f} = ??? : (LazyList[A]^{xs, f}) // error
    new Mapped

