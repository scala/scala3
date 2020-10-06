package dotty.dokka.model

import org.jetbrains.dokka.model.Bound
import org.jetbrains.dokka.links.DRI
import dotty.dokka._
case class HierarchyDiagram(edges: Seq[Edge])
case class Vertex(val id: Int, val body: DRIWithKind | BoundWithKind)
case class Edge(val from: Vertex, val to: Vertex)
