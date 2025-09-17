package unsafe

import scala.annotation.*

type XtoY = [X] =>> [Y] =>> X => Y

class Foo[T[_]]

class A[T] extends Annotation

object Bar:
  def ff(f: AnyRef => String, g: AnyRef ?=> Int): (AnyRef => String) = ???
  var x: Int = 0
  var y: String = ""
  def f[T <: Int](x: T): T = x
  def g[T <: AnyRef](x: T): T = x
  def g2[T >: Null <: AnyRef](x: T): T = x
  def h(x: String)(using Foo[Option]): String = x
  def h2(a: Foo[XtoY[String]]) = ???

class Bar2[T]:
  def f(x: T): T = x