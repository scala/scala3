import language.experimental.saferExceptions
import scala.compiletime.uninitialized
import scala.annotation.unchecked.uncheckedCaptures

trait LzyList[+A]:
  def isEmpty: Boolean
  def head: A
  def tail: LzyList[A]^{this}

object LzyNil extends LzyList[Nothing]:
  def isEmpty = true
  def head = ???
  def tail = ???

def LzyCons[A, C^](hd: A, tl: () => LzyList[A]^{C^}): LzyList[A]^{tl, C^} = new LzyList[A]:
  private var forced = false
  private var cache: LzyList[A @uncheckedCaptures]^{this, C^} = uninitialized
  private def force =
    if !forced then { cache = tl(); forced = true }
    cache

  def isEmpty = false
  def head = hd
  def tail: LzyList[A]^{this} = force
end LzyCons

extension [A](xs: LzyList[A]^)
  def map[B](f: A => B): LzyList[B]^{xs, f} =
    if xs.isEmpty then LzyNil
    else LzyCons(f(xs.head), () => xs.tail.map(f))

  def filter(p: A => Boolean): LzyList[A]^{xs, p} =
    if xs.isEmpty then LzyNil
    else if p(xs.head) then lazyCons(xs.head, xs.tail.filter(p))
    else xs.tail.filter(p)

  def concat(ys: LzyList[A]^): LzyList[A]^{xs, ys} =
    if xs.isEmpty then ys
    else xs.head #: xs.tail.concat(ys)

  def drop(n: Int): LzyList[A]^{xs} =
    if n == 0 then xs else xs.tail.drop(n - 1)
end extension

extension [A](x: A)
  def #:(xs1: => LzyList[A]^): LzyList[A]^{xs1} =
    LzyCons(x, () => xs1)

def lazyCons[A](x: A, xs1: => LzyList[A]^): LzyList[A]^{xs1} =
  LzyCons(x, () => xs1)

def tabulate[A](n: Int)(gen: Int => A): LzyList[A]^{gen} =
  def recur(i: Int): LzyList[A]^{gen} =
    if i == n then LzyNil
    else gen(i) #: recur(i + 1)
  recur(0)

class Ex1 extends Exception
class Ex2 extends Exception

def test(using cap1: CanThrow[Ex1], cap2: CanThrow[Ex2]) =
  val xs = 1 #: LzyNil

  def f(x: Int): Int throws Ex1 =
    if x < 0 then throw Ex1()
    x * x

  def g(x: Int): Int throws Ex1 =
    if x < 0 then throw Ex1()
    x * x

  def x1 = xs.map(f)
  def x1c: LzyList[Int]^{cap1} = x1

  def x2 = x1.concat(xs.map(g).filter(_ > 0))
  def x2c: LzyList[Int]^{cap1, cap2} = x2

  val x3 = tabulate(10) { i =>
      if i > 9 then throw Ex1()
      i * i
    }
  val x3c: LzyList[Int]^{cap1} = x3

class LimitExceeded extends Exception

def test2(n: Int)(using ct: CanThrow[LimitExceeded])  =
  val xs = tabulate(10) { i =>
    if i > 9 then throw LimitExceeded()
    i * i
  }
