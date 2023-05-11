object Test:
  def op[O, P](ta: List[O], tb: List[P]): List[P] = ???

  class Graph { class Node }

  def outsQ(using g: Graph): List[List[g.Node]] = ???

  object aGraph extends Graph
  given implA: aGraph.type = aGraph

  val q1: List[List[aGraph.Node]] = op(outsQ, op(outsQ, outsQ))
  implicitly[q1.type <:< List[List[aGraph.Node]]]

  val a1 = outsQ
  val a2 = op(outsQ, outsQ)
  val a3 = op(a1, a2)

  val q2 = op(outsQ, op(outsQ, outsQ))
  val q3: List[List[aGraph.Node]] = q2
  implicitly[q2.type <:< List[List[aGraph.Node]]]


