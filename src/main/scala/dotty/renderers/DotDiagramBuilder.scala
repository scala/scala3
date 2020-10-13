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
            s"""node${vertex.id} [label="${getHtmlLabel(vertex, sourceSetRestriciton, pageContext)}", style="${getStyle(vertex)}"];\n"""
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

    private def getStyle(vertex: Vertex) = vertex.body match {
        case DRIWithKind(_, kind) => kind
        case BoundWithKind(_, kind) => kind
    } match {
        case Kind.Class => "fill: #45AD7D;"
        case Kind.Object => "fill: #285577;"
        case Kind.Trait => "fill: #1CAACF;"
        case Kind.Enum => "fill: #B66722;"
    }

    private def getHtmlLabel(vertex: Vertex, sourceSetRestriciton: JSet[DisplaySourceSet], pageContext: ContentPage): String = "<span style='color: #FFFFFF;'>" + vertex.body.match {
        case DRIWithKind(dri, kind) => s"${kind.name} <a style='color: #FFFFFF;text-decoration: underline;' href=${locationProvider.resolve(dri, sourceSetRestriciton, pageContext)}>${dri.getClassNames}</a>"
        case BoundWithKind(bound, kind) => contentForBound(bound, kind, locationProvider, sourceSetRestriciton, pageContext)
    } + "</span>"

    private def contentForBound(
        b: Bound,
        k: Kind,
        locationProvider: LocationProvider,
        sourceSetRestriciton: JSet[DisplaySourceSet],
        pageContext: ContentPage
    ): String = b match {
            case t: org.jetbrains.dokka.model.TypeConstructor => t.getProjections.asScala.foldLeft(s"${k.name} ") {
                case (acc, p) => acc + (p match {
                    case text: UnresolvedBound => text.getName
                    case link: TypeParameter => {
                        val resolvedLink = locationProvider.resolve(link.getDri, sourceSetRestriciton, pageContext)
                        if resolvedLink != null then s"<a style='color: #FFFFFF;text-decoration: underline;' href=${resolvedLink}>${link.getName}</a>" else link.getName
                    }
                    case other => s"TODO: $other"
                })
            } 
            case o => s"TODO: $o"
    }
}