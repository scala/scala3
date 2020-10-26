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
        val vWithId = diagram.verteciesWithId
        val vertecies = vWithId.map { (vertex, id) =>
            s"""node${id} [label="${getHtmlLabel(vertex, renderer)}", style="${getStyle(vertex)}"];\n"""
        }.mkString

        val edges = diagram.edges.map { (from, to) =>
            s"""node${vWithId(from)} -> node${vWithId(to)};\n"""
        }.mkString

        s""" digraph g {
        | $vertecies
        |
        | $edges
        |}
        |""".stripMargin


    private def getStyle(vertex: LinkToType) = vertex.kind match 
        case Kind.Class => "fill: #45AD7D;"
        case Kind.Object => "fill: #285577;"
        case Kind.Trait => "fill: #1CAACF;"
        case Kind.Enum => "fill: #B66722;"
        case Kind.EnumCase => "fill: #B66722;"
        case other => sys.error(s"unexpected value: $other")


    private def getHtmlLabel(vertex: LinkToType, renderer: SignatureRenderer): String =
        span(style := "color: #FFFFFF;")(
            vertex.kind.name,
            " ",
            vertex.signature.map(renderer.renderElementWith(_))
        ).toString.replace("\"", "\\\"")
