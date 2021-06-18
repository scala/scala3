type Traverser[-I, +O] = I => LazyList[(O)]
extension[I, O](ta: Traverser[I, O])
    def ~>[P](tb: Traverser[O, P]): Traverser[I, P] = ???

class Graph { class Node }

case class Path[+E](e: E)
type Query[-I, +O] = Traverser[Path[I], Path[O]]

def nodesQ(using g: Graph): Query[Nothing, g.Node] = ???
def outsQ(using g: Graph): Query[g.Node, g.Node] = ???

object graphObj extends Graph
import graphObj._
given graphObj.type = graphObj

object Issue11556:
  val q1: Query[Nothing, Node] = nodesQ ~> outsQ
  implicitly[q1.type <:< Query[Nothing, Node]]

  val q2 = nodesQ ~> outsQ
  val q3: Query[Nothing, Node] = q2
  implicitly[q2.type <:< Query[Nothing, Node]]
end Issue11556
