package p
import scala.language.experimental.mode

class A(f: Int => Int):
  def foo(f: Int => Int) = ???
  def map(other: Iter): Iter = other
  def pair[T](x: T): (T, T) = (x, x)
