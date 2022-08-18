sealed trait Tree[+A]
final case class Leaf[+B](b: B)       extends Tree[B]
final case class Node[+C](l: List[C]) extends Tree[List[C]]

// The original test case, minimised.
object Test:
  def meth[X](tree: Tree[X]): X = tree match
    case Leaf(v) => v // ok: Tree[X] vs Leaf[B], PTC: X >: B, max: Leaf[B] => Leaf[X], x: X
    case Node(x) =>
      // tree: Tree[X] vs Node[C] aka Tree[List[C]]
      // PTC: X >: List[C]
      // max: Node[C] => Node[Any], instantiating C := Any, which makes X >: List[Any]
      // adapt: List[String] <: X = OKwithGADTUsed; insert GADT cast asInstanceOf[X]
      List("boom") // error: Found: List[String] Required: X where: X is a type in method meth with bounds >: List[C$1]
      // after fix:
      // max: Node[C] => Node[C$1], instantiating C := C$1, a new symbol, so X >: List[C$1]
      // adapt: List[String] <: X = Fail, because String !<: C$1

  def main(args: Array[String]): Unit =
    val tree = Node(List(42))
    val res = meth(tree)
    assert(res.head == 42) // was: ClassCastException: class java.lang.String cannot be cast to class java.lang.Integer
