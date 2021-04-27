object ABug {

  class Graph { class Node }

  def ltol[O](tb: List[O]): List[O] = ???

  def gtoll(using g: Graph): List[List[g.Node]] = ???

  object graph extends Graph
  import graph.*
  given graph.type = graph

  val osq: List[List[Node]] = gtoll

  val r: List[List[Any ]] = ltol(gtoll)
  val q: List[List[Node]] = ltol(gtoll)

}