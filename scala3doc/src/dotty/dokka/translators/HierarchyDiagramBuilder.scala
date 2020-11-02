package dotty.dokka

import org.jetbrains.dokka.model.Bound
import org.jetbrains.dokka.links.DRI
import dotty.dokka.model._
import dotty.dokka.model.api._


object HierarchyDiagramBuilder {
    def build(m: Member): HierarchyDiagram = {
        val mainVertex = Vertex(0, m.asLink)

        val supertypesEdges = m.parents.zipWithIndex.map { case (member, index) =>
           Edge(mainVertex, Vertex(index + 1, member))
        }

        val subtypesEdges = m.knownChildren.zipWithIndex.map { case (member, index) =>
            Edge(Vertex(index + m.knownChildren.size + 1, member), mainVertex)
        }

        HierarchyDiagram(supertypesEdges ++ subtypesEdges)
    }
}
