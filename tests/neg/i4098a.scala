object App {
  import scala.reflect.Selectable.reflectiveSelectable

  def coerce[U, V](u: U): V = {
    type X = { val x: { type R >: U } }
    type Y = { val x: { type R = V } }
    lazy val z: X & Y = z
    val u1: z.x.R = u  // error: Object { type R >: U | V <: V } is not stable (with arrows under z.x)
    u1
  }

  def main(args: Array[String]): Unit = {
    val x: Int = coerce[String, Int]("a")
    println(x + 1)
  }
}
