sealed trait T
object T

case class A(x: Int, y: Int) extends T
case object B extends T

object Test extends App {

  case class AA[X >: Null <: AnyRef](x: X, y: X, z: String)
}