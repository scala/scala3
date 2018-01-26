class GBTree[B] {
  class Tree[A, B]; class Node[A, B](value: Node[A, B]) extends Tree[A, B]
  case class B[A, B]() extends Tree[A, B]  // error: double definition
}
