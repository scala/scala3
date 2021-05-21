class DoubleList {
  class Node(var prev: Node, var next: Node, data: Int)
  object sentinel extends Node(sentinel, sentinel, 0)  // error // error

  def insert(x: Int) = ???
}
