import language.experimental.saferExceptions

trait LazyList[+A]:
  this: {*} LazyList[A] =>

  def isEmpty: Boolean
  def head: A
  def tail: {this} LazyList[A]

object LazyNil extends LazyList[Nothing]:
  def isEmpty: Boolean = true
  def head = ???
  def tail = ???

final class LazyCons[+T](val x: T, val xs: () => {*} LazyList[T]) extends LazyList[T]:
  this: {*} LazyList[T] =>

  def isEmpty = false
  def head = x
  def tail: {this} LazyList[T] = xs()
end LazyCons

extension [A](x: A)
  def #:(xs1: => {*} LazyList[A]): {xs1} LazyList[A] =
    LazyCons(x, () => xs1)

def tabulate[A](n: Int)(gen: Int => A) =
  def recur(i: Int): {gen} LazyList[A] =
    if i == n then LazyNil
    else gen(i) #: recur(i + 1)
  recur(0)

class Ex1 extends Exception

def problem =
  try  // error
    tabulate(10) { i =>
      if i > 9 then throw Ex1()
      i * i
    }
  catch case ex: Ex1 => LazyNil




