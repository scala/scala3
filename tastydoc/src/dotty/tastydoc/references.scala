package dotty.tastydoc

object references {
  sealed trait Reference
  //Be aware that the label may end with a "$" in case it is referencing an object
  final case class TypeReference(label: String, path: String, typeParams: List[Reference], hasOwnFile: Boolean = false) extends Reference
  final case class OrTypeReference(left: Reference, right: Reference) extends Reference
  final case class AndTypeReference(left: Reference, right: Reference) extends Reference
  final case class FunctionReference(args: List[Reference], returnValue: Reference, isImplicit: Boolean) extends Reference
  final case class TupleReference(args: List[Reference]) extends Reference
  final case class BoundsReference(low: Reference, high: Reference) extends Reference
  final case class NamedReference(name: String, ref: Reference, isRepeated: Boolean = false) extends Reference
  final case class ByNameReference(ref: Reference) extends Reference
  final case class ConstantReference(label: String) extends Reference
  final case class CompanionReference(label: String, path: String, kind: String) extends Reference
  final case class RefinedReference(parent: Reference, ls: List[(String, String, Reference)]) extends Reference
  case object EmptyReference extends Reference
}