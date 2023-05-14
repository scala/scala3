sealed trait Tree[+A]
final case class Leaf[+B](b: B)       extends Tree[B]
final case class Node[+C](l: List[C]) extends Tree[List[C]]

// A version of the original test case that is sound so should typecheck.
object Test:
  def meth[X](tree: Tree[X]): X = tree match
    case Leaf(v) => v // ok: Tree[X] vs Leaf[B], PTC: X >: B,       max: Leaf[B] => Leaf[X],   x: X   <:< X
    case Node(x) => x // ok: Tree[X] vs Node[C], PTC: X >: List[C], max: Node[C] => Node[C$1], x: C$1 <:< X, w/ GADT cast

  def main(args: Array[String]): Unit =
    val tree = Node(List(42))
    val res = meth(tree)
    assert(res.head == 42) // ok
