class CC
type Cap = CC^

trait LazyList[+A]:
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
      this: Mapped^{xs, f} =>

      def isEmpty = false
      def head: B = f(xs.head)
      def tail: LazyList[B]^{this} = xs.tail.map(f)  // OK
    if xs.isEmpty then LazyNil
    else new Mapped

def test(cap1: Cap, cap2: Cap) =
  def f(x: String): String = if cap1 == cap1 then "" else "a"
  def g(x: String): String = if cap2 == cap2 then "" else "a"

  val xs =
    class Initial extends LazyList[String]:
      this: Initial^{cap1} =>

      def isEmpty = false
      def head = f("")
      def tail = LazyNil
    new Initial
  val xsc: LazyList[String]^{cap1} = xs
  val ys = xs.map(g)
  val ysc: LazyList[String]^{cap1, cap2} = ys
