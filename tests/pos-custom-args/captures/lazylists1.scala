class CC
type Cap = {*} CC

trait LazyList[+A]:
  this: {*} LazyList[A] =>

  def isEmpty: Boolean
  def head: A
  def tail: {this} LazyList[A]
  def concat[B >: A](other: {*} LazyList[B]): {this, other} LazyList[B]

object LazyNil extends LazyList[Nothing]:
  def isEmpty: Boolean = true
  def head = ???
  def tail = ???
  def concat[B](other: {*} LazyList[B]): {other} LazyList[B] = other

final class LazyCons[+A](x: A)(xs: () => {*} LazyList[A]) extends LazyList[A]:
  this: {*} LazyList[A] =>

  def isEmpty = false
  def head = x
  def tail: {this} LazyList[A] = xs()
  def concat[B >: A](other: {*} LazyList[B]): {this, other} LazyList[B] =
    LazyCons(head)(() => tail.concat(other))

extension [A](xs: {*} LazyList[A])
  def map[B](f: A => B): {xs, f} LazyList[B] =
    if xs.isEmpty then LazyNil
    else LazyCons(f(xs.head))(() => xs.tail.map(f))

def test(cap1: Cap, cap2: Cap) =
  def f(x: String): String = if cap1 == cap1 then "" else "a"
  def g(x: String): String = if cap2 == cap2 then "" else "a"

  val xs = new LazyCons("")(() => if f("") == f("") then LazyNil else LazyNil)
  val xsc: {cap1} LazyList[String] = xs
  val ys = xs.map(g)
  val ysc: {cap1, cap2} LazyList[String] = ys
  val zs = new LazyCons("")(() => if g("") == g("") then LazyNil else LazyNil)
  val as = xs.concat(zs)
  val asc: {xs, zs} LazyList[String] = as
