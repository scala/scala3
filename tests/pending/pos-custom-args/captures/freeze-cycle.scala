import caps.*

class Node(val payload: Int) extends Mutable:
  var next: Node = ???


type INode = Node^{}

def cycle(x: Int, y: Int): (INode, INode) =
  freeze:
    val a = Node(x)
    val b = Node(y)
    a.next = b
    b.next = a
    (a, b)
