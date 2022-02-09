import language.experimental.saferExceptions
import scala.compiletime.uninitialized

trait LzyList[+A]:
  def isEmpty: Boolean
  def head: A
  def tail: {this} LzyList[A]

object LzyNil extends LzyList[Nothing]:
  def isEmpty = true
  def head = ???
  def tail = ???

final class LzyCons[+A](hd: A, tl: () => {*} LzyList[A]) extends LzyList[A]:
  private var forced = false
  private var cache: {this} LzyList[A] = uninitialized
  private def force =
    if !forced then { cache = tl(); forced = true }
    cache

  def isEmpty = false
  def head = hd
  def tail: {this} LzyList[A] = force
end LzyCons

extension [A](xs: {*} LzyList[A])
  def map[B](f: A => B): {xs, f} LzyList[B] =
    if xs.isEmpty then LzyNil
    else LzyCons(f(xs.head), () => xs.tail.map(f))

  def filter(p: A => Boolean): {xs, p} LzyList[A] =
    if xs.isEmpty then LzyNil
    else if p(xs.head) then lazyCons(xs.head, xs.tail.filter(p))
    else xs.tail.filter(p)

  def concat(ys: {*} LzyList[A]): {xs, ys} LzyList[A] =
    if xs.isEmpty then ys
    else xs.head #: xs.tail.concat(ys)

  def drop(n: Int): {xs} LzyList[A] =
    if n == 0 then xs else xs.tail.drop(n - 1)
end extension

extension [A](x: A)
  def #:(xs1: => {*} LzyList[A]): {xs1} LzyList[A] =
    LzyCons(x, () => xs1)

def lazyCons[A](x: A, xs1: => {*} LzyList[A]): {xs1} LzyList[A] =
  LzyCons(x, () => xs1)

def tabulate[A](n: Int)(gen: Int => A) =
  def recur(i: Int): {gen} LzyList[A] =
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
  def x1c: {cap1} LzyList[Int] = x1

  def x2 = x1.concat(xs.map(g).filter(_ > 0))
  def x2c: {cap1, cap2} LzyList[Int] = x2

  val x3 = tabulate(10) { i =>
      if i > 9 then throw Ex1()
      i * i
    }
  val x3c: {cap1} LzyList[Int] = x3

class LimitExceeded extends Exception

def test2(n: Int)(using ct: CanThrow[LimitExceeded])  =
  val xs = tabulate(10) { i =>
    if i > 9 then throw LimitExceeded()
    i * i
  }
