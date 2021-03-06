import util.chaining.scalaUtilChainingOps
object Test extends App {
  def x = 42.tap(println(_))
  def y = 27.tap(println(_))
  def z = 17.tap(println(_))
  def f(i: Int = x, j: Int = y) = i + j
  f(j = z)
}