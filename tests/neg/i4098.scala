object App {
  import scala.reflect.Selectable.reflectiveSelectable

  def coerce[U, V](u: U): V = {
    type X = { type R >: U }
    type Y = { type R = V }
    type Z = X & Y
    val u1: Z#R = u // error: not a legal path
    u1
  }

  def main(args: Array[String]): Unit = {
    val x: Int = coerce[String, Int]("a")
    println(x + 1)
  }
}
