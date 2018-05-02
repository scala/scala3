class List {
  val sentinel: Node = new Node(null, null, this, null)  // error

  def insert(data: AnyRef) = sentinel.insertAfter(data)
}

class Node(var prev: Node, var next: Node, parent: List, data: AnyRef) {
  def insertAfter(data: AnyRef) = {
    val node = new Node(this, this.next, this.parent, data)
    next.prev = node
    next = node
  }
}