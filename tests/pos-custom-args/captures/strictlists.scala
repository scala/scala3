
trait StrictList[+A]:
  def isEmpty: Boolean
  def head: A
  def tail: StrictList[A]

object StrictNil extends StrictList[Nothing]:
  def isEmpty = true
  def head = ???
  def tail = ???

final class StrictCons[+A](hd: A, tl: StrictList[A]) extends StrictList[A]:
  def isEmpty = false
  def head = hd
  def tail: StrictList[A] = tl
end StrictCons

extension [A](xs: StrictList[A])
  def map[B](f: A => B): StrictList[B] =
    if xs.isEmpty then StrictNil
    else StrictCons(f(xs.head),xs.tail.map(f))

  def filter(p: A => Boolean): StrictList[A] =
    if xs.isEmpty then StrictNil
    else if p(xs.head) then xs.head #: xs.tail.filter(p)
    else xs.tail.filter(p)

  def concat(ys: StrictList[A]): StrictList[A] =
    if xs.isEmpty then ys
    else xs.head #: xs.tail.concat(ys)
end extension

extension [A](x: A)
  def #:(xs1: StrictList[A]): StrictList[A] =
    StrictCons(x, xs1)

def tabulate[A](n: Int)(gen: Int => A) =
  def recur(i: Int): StrictList[A] =
    if i == n then StrictNil
    else gen(i) #: recur(i + 1)
  recur(0)