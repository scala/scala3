import scala.annotation.tailrec
class Context {
  type Tree
}

final def loop3[C <: Context](): Unit =
  @tailrec
  def loop4[A <: C](c: A): c.Tree = loop4(c)