
import annotation.*

class C(@constructorOnly s: String): // error
  def g: Any => String = _ => s
  def f[A](xs: List[A]): List[String] = xs.map(g)

import scala.util.boundary

class Leak()(using @constructorOnly l: boundary.Label[String]): // error
  lazy val broken = Option("stop").foreach(boundary.break(_))
