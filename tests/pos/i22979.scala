
import annotation.*

class C(@constructorOnly s: String):
  val g: Any => String = _ => s
  def f[A](xs: List[A]): List[String] = xs.map(g)

  val gg: Any => Any = (x: Any) => (y: Any) => s

  (y => s): (Any => Any) // pure expr
  println(s)
end C

import scala.util.boundary

class Leak()(using @constructorOnly l: boundary.Label[String]):
  Option("stop").foreach(boundary.break(_))


class Lapse:
  def f = Lapse.DefaultSentinelFn()
object Lapse:
  private val DefaultSentinel: AnyRef = new AnyRef
  private val DefaultSentinelFn: () => AnyRef = () => DefaultSentinel

class scalafan(@constructorOnly str: String) {
  val scream = Array.fill(10)(str + "!")
  def screamTenTimes: Unit = scream.foreach(println)
}
