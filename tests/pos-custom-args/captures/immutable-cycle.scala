import caps.*

class Node[All](val payload: Int) extends Modifiable:
  var next: Node = ???


type INode = Node^{}

def cycle(x: Int, y: Int): (INode, INode) =
  immutable:
    val a = Node(x)
    val b = Node(y)
    a.next = b
    b.next = a
    (a, b)
