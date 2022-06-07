package dotty.tools.scaladoc
package renderers

import util.HTML._

object DotDiagramBuilder:
  def build(diagram: HierarchyGraph, renderer: SignatureRenderer)(using DocContext): String =
    def getClasses(kind: Kind): String =
      val kindClass = kind match
        case _ : Kind.Class => "class"
        case Kind.Object => "object"
        case _ : Kind.Trait => "trait"
        case e if e.isInstanceOf[Kind.Enum] => "enum"
        case e if e.isInstanceOf[Kind.EnumCase] => "enumcase"
        case other => report.error(s"unexpected value: $other")
      s"$kindClass vertex"

    val vWithId = diagram.verteciesWithId
    val sealedNodes = diagram.sealedNodes
    val vertecies = vWithId.toList.sortBy((_, id) => id).map { (vertex, id) =>
      s"""node${id} [id=node${id}, label="${getHtmlLabel(vertex, renderer, sealedNodes)}", class="${getClasses(vertex.kind)}"];\n"""
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
