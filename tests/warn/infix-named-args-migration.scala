//> using options -source:3.6-migration
import scala.language.experimental.namedTuples

class C:
  def f = 42 + (x = 1) // warn // interpreted as 42.+(x = 1) under migration, x is a valid synthetic parameter name
  def multi(x: Int, y: Int): Int = x + y
  def **(x: Int, y: Int): Int = x + y
  def g = new C() `multi` (x = 42, y = 27) // warn
  def h = new C() ** (x = 42, y = 27) // warn

type X = (x: Int)

class D(d: Int):
  def **(x: Int): Int = d * x
  def f = this ** (x = 2) // warn
  def g = this ** 2
