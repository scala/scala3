class Dummy
object Dummy:
  val empty = new Dummy

sealed trait Node:
  def as[T](using d: Dummy = Dummy.empty): Either[String, T] = ???

object Sample extends App {
  val node: Node = ???

  val x: Either[Int, Any] = node.as[Any] // error
}