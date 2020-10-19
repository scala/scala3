package dotty.dokka

import dotty.dokka.model._
import org.jetbrains.dokka.model._
import org.jetbrains.dokka.links.DRI
import org.jetbrains.dokka.base.resolvers.local.LocationProvider
import org.jetbrains.dokka.pages._
import dotty.dokka.model.api.Kind
import HTML._
import dotty.dokka.model.api._

object DotDiagramBuilder:
    def build(diagram: HierarchyGraph, renderer: SignatureRenderer): String = 
        val vertecies = diagram.edges.flatMap(edge => Seq(edge.from, edge.to)).distinct.map { vertex =>
            s"""node${vertex.id} [label="${getHtmlLabel(vertex, renderer)}", style="${getStyle(vertex)}"];\n"""
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


    private def getStyle(vertex: Vertex) = vertex.body.kind match
        case Kind.Class => "fill: #45AD7D;"
        case Kind.Object => "fill: #285577;"
        case Kind.Trait => "fill: #1CAACF;"
        case Kind.Enum => "fill: #B66722;"
        case Kind.EnumCase => "fill: #B66722;"
        case other => sys.error(s"unexpected value: $other")


    private def getHtmlLabel(vertex: Vertex, renderer: SignatureRenderer): String =
        span(style := "color: #FFFFFF;")(
            vertex.body.kind.name,
            " ",
            vertex.body.signature.map(renderer.renderElementWith(_))
        ).toString.replace("\"", "\\\"")
