sealed trait Tree[+A]
final case class Leaf[+B](v: B)        extends Tree[B]
final case class Node[+C](xs: List[C]) extends Tree[List[C]]

object Test:
  def meth[X](tree: Tree[X]): X = tree match
    case Leaf(v) => v
    case Node(x) => List("boom") // error

  def main(args: Array[String]): Unit =
    val tree = Node(List(42))
    val res = meth(tree)
    assert(res.head == 42) // was: ClassCastException: class java.lang.String cannot be cast to class java.lang.Integer
