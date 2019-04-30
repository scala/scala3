package tastydoc

object references {
  sealed trait Reference
  final case class TypeReference(label: String, link: String, typeParams: List[Reference]) extends Reference
  final case class OrTypeReference(left: Reference, right: Reference) extends Reference
  final case class AndTypeReference(left: Reference, right: Reference) extends Reference
  final case class FunctionReference(args: List[Reference], returnValue: Reference, isImplicit: Boolean) extends Reference
  final case class TupleReference(args: List[Reference]) extends Reference
  final case class BoundsReference(low: Reference, high: Reference) extends Reference
  //final case class NamedReference(label: String, ref: Reference, isByName: Boolean = false, isRepeated: Boolean = false) extends Reference
  final case class ConstantReference(label: String) extends Reference
  case object EmptyReference extends Reference
}