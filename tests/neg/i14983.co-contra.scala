case class Showing[-C](show: C => String)

sealed trait Tree[+A]
final case class Leaf[+B](b: B)       extends Tree[B]
final case class Node[-C](l: Showing[C]) extends Tree[Showing[C]]

object Test:
  def meth[X](tree: Tree[X]): X = tree match
    case Leaf(v) => v
    case Node(x) =>
      // tree: Tree[X] vs Node[C] aka Tree[Showing[C]]
      // PTC: X >: Showing[C]
      // max: Node[C] to Node[Nothing], instantiating C := Nothing, which makes X >: Showing[Nothing]
      // adapt: Showing[String] <: X = OKwithGADTUsed; insert GADT cast asInstanceOf[X]
      Showing[String](_ + " boom!") // error: Found: Showing[String] Required: X where: X is a type in method meth with bounds >: Showing[C$1]
      // after fix:
      // max: Node[C] => Node[C$1], instantiating C := C$1, a new symbol, so X >: Showing[C$1]
      // adapt: Showing[String] <: X = Fail, because String !<: C$1

  def main(args: Array[String]): Unit =
    val tree = Node(Showing[Int](_.toString))
    val res = meth(tree)
    println(res.show(42)) // was: ClassCastException: class java.lang.Integer cannot be cast to class java.lang.String
