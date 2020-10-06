package dotty.dokka

import dotty.dokka.model._
import dotty.dokka.Kind
import org.jetbrains.dokka.model._
import org.jetbrains.dokka.links.DRI
import org.jetbrains.dokka.base.resolvers.local.LocationProvider
import org.jetbrains.dokka.pages._
import java.util.{Set => JSet}
import collection.JavaConverters._

class DotDiagramBuilder(val locationProvider: LocationProvider) {
    def build(diagram: HierarchyDiagram, sourceSetRestriciton: JSet[DisplaySourceSet], pageContext: ContentPage): String = {

        val vertecies = diagram.edges.flatMap(edge => Seq(edge.from, edge.to)).distinct.map { vertex =>
            s"""node${vertex.id} [label="${getHtmlLabel(vertex, sourceSetRestriciton, pageContext)}"];\n"""
        }.mkString

        val edges = diagram.edges.map { edge =>
            s"""node${edge.from.id} -> node${edge.to.id};\n"""
        }.mkString

        s""" digraph g {
        | $vertecies
        |
        | $edges
        |}
        |""".stripMargin
    }

    private def getHtmlLabel(vertex: Vertex, sourceSetRestriciton: JSet[DisplaySourceSet], pageContext: ContentPage): String = vertex.body match {
        case DRIWithKind(dri, kind) => s"${kind.toString.toLowerCase} <a href=${locationProvider.resolve(dri, sourceSetRestriciton, pageContext)}>${dri.getClassNames}</a>"
        case BoundWithKind(bound, kind) => contentForBound(bound, kind, locationProvider, sourceSetRestriciton, pageContext)
    }
    

    def contentForBound(
        b: Bound,
        k: Kind,
        locationProvider: LocationProvider,
        sourceSetRestriciton: JSet[DisplaySourceSet],
        pageContext: ContentPage
    ): String = b match {
            case t: org.jetbrains.dokka.model.TypeConstructor => t.getProjections.asScala.foldLeft(s"${k.toString.toLowerCase} "){
                case (acc, p) => acc + (p match {
                    case text: UnresolvedBound => text.getName
                    case link: TypeParameter => {
                        val resolvedLink = locationProvider.resolve(link.getDri, sourceSetRestriciton, pageContext)
                        if resolvedLink != null then s"<a href=${resolvedLink}>${link.getName}</a>" else link.getName
                    }
                    case other => s"TODO: $other"
                })
            }
            case o => s"TODO: $o"
    }
}