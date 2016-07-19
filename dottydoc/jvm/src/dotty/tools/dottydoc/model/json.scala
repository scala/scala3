package dotty.tools.dottydoc
package model

import model.comment._

/** This object provides a protocol for serializing the package AST to JSON
 *
 *  TODO: It might be a good ideat to represent the JSON better than just
 *  serializing a big string-blob in the future.
 */
object json {
  implicit class JsonString(val str: String) extends AnyVal {
    def json: String = {
      val cleanedString = str
        .replaceAll("\\\\","\\\\\\\\")
        .replaceAll("\\\"", "\\\\\"")
        .replaceAll("\n", "\\\\n")

      s""""$cleanedString""""
    }
  }

  implicit class JsonBoolean(val boo: Boolean) extends AnyVal {
    def json: String = if (boo) "true" else "false"
  }

  implicit class JsonComment(val cmt: Comment) extends AnyVal {
    def json: String = s"""{"body":${cmt.body.json},"short":${cmt.short.json}}"""
  }

  implicit class LinkJson(val link: MaterializableLink) extends AnyVal {
    def json: String = {
      val (secondTitle, secondValue, kind) = link match {
        case ul: UnsetLink => ("query".json, ul.query.json, "UnsetLink".json)
        case ml: MaterializedLink => ("target", ml.target.json, "MaterializedLink".json)
        case nl: NoLink => ("target".json, nl.target.json, "NoLink".json)
      }
      s"""{"title":${link.title.json},$secondTitle:${secondValue},"kind":$kind}"""
    }
  }

  private def refToJson(ref: Reference): String = ref match {
    case ref: TypeReference =>
      s"""{"title":${ref.title.json},"tpeLink":${ref.tpeLink.json},"paramLinks":${ref.paramLinks.map(_.json).mkString("[",",","]")},"kind":"TypeReference"}"""
    case ref: AndTypeReference =>
      s"""{"left":${refToJson(ref.left)},"right":${refToJson(ref.right)},"kind":"AndTypeReference"}"""
    case ref: OrTypeReference =>
      s"""{"left":${refToJson(ref.left)},"right":${refToJson(ref.right)},"kind":"OrTypeReference"}"""
    case ref: BoundsReference =>
      s"""{"low":${refToJson(ref.low)},"high":${refToJson(ref.high)},"kind":"BoundsReference"}"""
    case ref: NamedReference =>
      s"""{"title":${ref.title.json},"ref":${refToJson(ref.ref)},"isByName":${ref.isByName.json},"kind":"NamedReference"}"""
    case ref: ConstantReference =>
      s"""{"title":${ref.title.json},"kind": "ConstantReference"}"""
    case ref: FunctionReference =>
      s"""{"args":${ref.args.map(refToJson).mkString("[",",","]")},"returnValue":${refToJson(ref.returnValue)},"kind": "FunctionReference"}"""
  }
  implicit class ReferenceJson(val ref: Reference) extends AnyVal { def json: String = refToJson(ref) }

  private def entToJson(ent: Entity): String = ent match {
    case ent: Package =>
      s"""{"name":${ent.name.json},"members":${ent.members.map(_.json).mkString("[",",","]")},"path":${ent.path.map(_.json).mkString("[",",","]")},${ent.comment.map(_.json).fold("")(cmt => s""""comment":$cmt,""")}"kind":"package"}"""
    case ent: Class =>
      s"""{"name":${ent.name.json},"members":${ent.members.map(_.json).mkString("[",",","]")},"modifiers":${ent.modifiers.map(_.json).mkString("[",",","]")},"path":${ent.path.map(_.json).mkString("[",",","]")},"typeParams":${ent.typeParams.map(_.json).mkString("[",",","]")},"superTypes":${ent.superTypes.map(_.json).mkString("[",",","]")},${ent.comment.map(_.json).fold("")(cmt => s""""comment":$cmt,""")}"kind":"class"}"""
    case ent: CaseClass =>
      s"""{"name":${ent.name.json},"members":${ent.members.map(_.json).mkString("[",",","]")},"modifiers":${ent.modifiers.map(_.json).mkString("[",",","]")},"path":${ent.path.map(_.json).mkString("[",",","]")},"typeParams":${ent.typeParams.map(_.json).mkString("[",",","]")},"superTypes":${ent.superTypes.map(_.json).mkString("[",",","]")},${ent.comment.map(_.json).fold("")(cmt => s""""comment":$cmt,""")}"kind":"case class"}"""
    case ent: Trait =>
      s"""{"name":${ent.name.json},"members":${ent.members.map(_.json).mkString("[",",","]")},"modifiers":${ent.modifiers.map(_.json).mkString("[",",","]")},"path":${ent.path.map(_.json).mkString("[",",","]")},"typeParams":${ent.typeParams.map(_.json).mkString("[",",","]")},"superTypes":${ent.superTypes.map(_.json).mkString("[",",","]")},${ent.comment.map(_.json).fold("")(cmt => s""""comment":$cmt,""")}"kind":"trait"}"""
    case ent: Object =>
      s"""{"name":${ent.name.json},"members":${ent.members.map(_.json).mkString("[",",","]")},"modifiers":${ent.modifiers.map(_.json).mkString("[",",","]")},"path":${ent.path.map(_.json).mkString("[",",","]")},"superTypes":${ent.superTypes.map(_.json).mkString("[",",","]")},${ent.comment.map(_.json).fold("")(cmt => s""""comment":$cmt,""")}"kind":"object"}"""
    case ent: Def =>
      s"""{"name":${ent.name.json},"modifiers":${ent.modifiers.map(_.json).mkString("[",",","]")},"path":${ent.path.map(_.json).mkString("[",",","]")},"returnValue":${ent.returnValue.json},"typeParams":${ent.typeParams.map(_.json).mkString("[",",","]")},"paramLists":${ent.paramLists.map{ xs =>xs.map(_.json).mkString("[",",","]")}.mkString("[",",","]")},${ent.comment.map(_.json).fold("")(cmt => s""""comment":$cmt,""")}"kind":"def"}"""
    case ent: Val =>
      s"""{"name":${ent.name.json},"modifiers":${ent.modifiers.map(_.json).mkString("[",",","]")},"path":${ent.path.map(_.json).mkString("[",",","]")},"returnValue":${ent.returnValue.json},${ent.comment.map(_.json).fold("")(cmt => s""""comment":$cmt,""")}"kind":"val"}"""
  }
  implicit class EntityJson(val ent: Entity) extends AnyVal { def json: String = entToJson(ent) }
  implicit class PackageJson(val pack: Package) extends AnyVal { def json: String = (pack: Entity).json }

  implicit class PackMapJson(val packs: Map[String, Package]) extends AnyVal {
    def json: String = packs
      .map { case (k, v) => s"${k.json}: ${v.json}" }
      .mkString("{",",","}")
  }
}
