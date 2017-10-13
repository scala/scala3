package dotty.tools.dottydoc
package model

object references {
  sealed trait Reference
  final case class TypeReference(title: String, tpeLink: MaterializableLink, paramLinks: List[Reference]) extends Reference
  final case class OrTypeReference(left: Reference, right: Reference) extends Reference
  final case class AndTypeReference(left: Reference, right: Reference) extends Reference
  final case class FunctionReference(args: List[Reference], returnValue: Reference) extends Reference
  final case class TupleReference(args: List[Reference]) extends Reference
  final case class BoundsReference(low: Reference, high: Reference) extends Reference
  final case class NamedReference(title: String, ref: Reference, isByName: Boolean = false, isRepeated: Boolean = false) extends Reference
  final case class ConstantReference(title: String) extends Reference
  final case object EmptyReference extends Reference

  /** Use MaterializableLink for entities that need be picklable */
  sealed trait MaterializableLink { def title: String }
  final case class UnsetLink(title: String, query: String) extends MaterializableLink
  final case class MaterializedLink(title: String, target: String) extends MaterializableLink {
    def this(title: String, target: Entity) = this(title, target match {
      case target: Package =>
        target.path.mkString("/") + "/index.html"
      case _: TypeAlias | _: Def | _: Val =>
        target.parent.path.mkString("/") + ".html#" + target.signature
      case _ =>
        target.path.mkString("/") + ".html"
    })
  }
  final case class NoLink(title: String, target: String) extends MaterializableLink

  implicit class ReferenceShower(val ref: Reference) extends AnyVal {
    def showReference: String = ref match {
      case TypeReference(title, _, tparams) =>
        title + {
          if (tparams.nonEmpty) tparams.map(_.showReference).mkString("[", ",", "]")
          else ""
        }

      case OrTypeReference(left, right) =>
        left.showReference + " | " + right.showReference
      case AndTypeReference(left, right) =>
        left.showReference + " &amp; " + right.showReference

      case FunctionReference(args, ret) =>
        if (args.isEmpty)
          "() => " + ret.showReference
        else if (args.tail.isEmpty)
          args.head.showReference + " => " + ret.showReference
        else
          args.mkString("(", ",", s") => ${ret.showReference}")

      case TupleReference(xs) =>
        xs.mkString("(", ",", ")")

      case BoundsReference(lo, hi) =>
        lo.showReference + "<: " + hi.showReference

      case NamedReference(title, ref, isByName, isRepeated) =>
        val byName = if (isByName) "=> " else ""
        val repeated = if (isRepeated) "*" else ""
        s"$title: $byName${ref.showReference}$repeated"

      case ConstantReference(title) => title
      case EmptyReference => ""
    }
  }
}
