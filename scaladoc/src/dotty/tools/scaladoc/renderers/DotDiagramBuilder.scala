package dotty.tools.scaladoc
package renderers

import util.HTML._

object DotDiagramBuilder:
  def build(diagram: HierarchyGraph, renderer: SignatureRenderer)(using DocContext): String =
    def getStyle(kind: Kind) = kind match
      case _ : Kind.Class => "fill: #45AD7D;"
      case Kind.Object => "fill: #285577;"
      case _ : Kind.Trait => "fill: #1CAACF;"
      case e if e.isInstanceOf[Kind.Enum] => "fill: #B66722;"
      case e if e.isInstanceOf[Kind.EnumCase] => "fill: #B66722;"
      case other => report.error(s"unexpected value: $other")

    val vWithId = diagram.verteciesWithId
    val sealedNodes = diagram.sealedNodes
    val vertecies = vWithId.map { (vertex, id) =>
      s"""node${id} [id=node${id}, label="${getHtmlLabel(vertex, renderer, sealedNodes)}", style="${getStyle(vertex.kind)}"];\n"""
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

  private def getHtmlLabel(vertex: LinkToType, renderer: SignatureRenderer, sealedNodes: Set[LinkToType]): String =
    span(style := "color: #FFFFFF;")(
      if sealedNodes.contains(vertex) then "sealed " else "",
      vertex.kind.name,
      " ",
      vertex.signature.map(renderer.renderElementWith(_))
    ).toString.replace("\"", "\\\"")
