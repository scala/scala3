
import annotation.*

class C(@constructorOnly s: String):
  val g: Any => String = _ => s
  def f[A](xs: List[A]): List[String] = xs.map(g)

import scala.util.boundary

class Leak()(using @constructorOnly l: boundary.Label[String]):
  Option("stop").foreach(boundary.break(_))
