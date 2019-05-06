package dotty.tastydoc

object references {
  sealed trait Reference
  final case class TypeReference(label: String, link: String, typeParams: List[Reference]) extends Reference
  final case class OrTypeReference(left: Reference, right: Reference) extends Reference
  final case class AndTypeReference(left: Reference, right: Reference) extends Reference
  final case class FunctionReference(args: List[Reference], returnValue: Reference, isImplicit: Boolean) extends Reference
  final case class TupleReference(args: List[Reference]) extends Reference
  final case class BoundsReference(low: Reference, high: Reference) extends Reference
  final case class NamedReference(name: String, ref: Reference, isRepeated: Boolean = false) extends Reference // TODO isRepeated
  final case class ByNameReference(ref: Reference) extends Reference
  final case class ConstantReference(label: String) extends Reference
  case object EmptyReference extends Reference
}