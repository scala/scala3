object NameKinds:
  abstract class NameKind(val tag: Int):
    class Info
  class QualifiedNameKind(tag: Int, val separator: String) extends NameKind(tag):
    qualifiedNameKinds(tag) = this // error
  

  val MAX_TAG = 8
  val qualifiedNameKinds = new Array[QualifiedNameKind](MAX_TAG)

  val QualifiedName: QualifiedNameKind = new QualifiedNameKind(0, ".")
  val FlatName: QualifiedNameKind = new QualifiedNameKind(1, "$")