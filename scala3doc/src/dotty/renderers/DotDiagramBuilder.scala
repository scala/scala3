package dotty.dokka

import dotty.dokka.model._
import HTML._
import dotty.dokka.model.api._

object DotDiagramBuilder:
  def build(diagram: HierarchyGraph, renderer: SignatureRenderer)(using DocContext): String =
    def getStyle(vertex: LinkToType) = vertex.kind match
      case _ : Kind.Class => "fill: #45AD7D;"
      case Kind.Object => "fill: #285577;"
      case _ : Kind.Trait => "fill: #1CAACF;"
      case Kind.Enum => "fill: #B66722;"
      case _ : Kind.EnumCase => "fill: #B66722;"
      case other => report.error(s"unexpected value: $other")

    val vWithId = diagram.verteciesWithId
    val vertecies = vWithId.map { (vertex, id) =>
      s"""node${id} [id=node${id}, label="${getHtmlLabel(vertex, renderer)}", style="${getStyle(vertex)}"];\n"""
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

  private def getHtmlLabel(vertex: LinkToType, renderer: SignatureRenderer): String =
    span(style := "color: #FFFFFF;")(
      vertex.kind.name,
      " ",
      vertex.signature.map(renderer.renderElementWith(_))
    ).toString.replace("\"", "\\\"")
