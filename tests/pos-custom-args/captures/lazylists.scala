class CC
type Cap = {*} CC

trait LazyList[+A]:
  this: {*} LazyList[A] =>

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
      this: {xs, f} Mapped =>

      def isEmpty = false
      def head: B = f(xs.head)
      def tail: {this} LazyList[B] = xs.tail.map(f)  // OK
    if xs.isEmpty then LazyNil
    else new Mapped

def test(cap1: Cap, cap2: Cap) =
  def f(x: String): String = if cap1 == cap1 then "" else "a"
  def g(x: String): String = if cap2 == cap2 then "" else "a"

  val xs =
    class Initial extends LazyList[String]:
      this: {cap1} Initial =>

      def isEmpty = false
      def head = f("")
      def tail = LazyNil
    new Initial
  val xsc: {cap1} LazyList[String] = xs
  val ys = xs.map(g)
  val ysc: {cap1, cap2} LazyList[String] = ys
