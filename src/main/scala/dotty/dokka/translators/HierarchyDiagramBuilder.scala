package dotty.dokka

import org.jetbrains.dokka.model.Bound
import org.jetbrains.dokka.links.DRI
import dotty.dokka.model._

object HierarchyDiagramBuilder {
    def build(mainType: DRIWithKind, supertypes: Seq[BoundWithKind], subtypes: Seq[DRIWithKind]): HierarchyDiagram = {

        val mainVertex = Vertex(0, mainType)

        val supertypesEdges = supertypes.zipWithIndex.map { case (bound, index) =>
           Edge(mainVertex, Vertex(index + 1, bound))
        }

        val subtypesEdges = subtypes.zipWithIndex.map { case (dri, index) =>
            Edge(Vertex(index + supertypes.size + 1, dri), mainVertex)
        }


        HierarchyDiagram(supertypesEdges ++ subtypesEdges)
    }
}