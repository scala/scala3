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

  /** Use MaterializableLink for entities that need be picklable */
  sealed trait MaterializableLink { def title: String }
  final case class UnsetLink(title: String, query: String) extends MaterializableLink
  final case class MaterializedLink(title: String, target: String) extends MaterializableLink
  final case class NoLink(title: String, target: String) extends MaterializableLink

  object AndOrTypeReference {
    def unapply(ref: Reference): Option[(Reference, String, Reference)] = ref match {
      case OrTypeReference(left, right) => Some((left, "|", right))
      case AndTypeReference(left, right) => Some((left, "&amp;", right))
      case _ => None
    }
  }
}
