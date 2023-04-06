import scala.collection.mutable

object NameKinds { // error
  private val qualifiedNameKinds = mutable.HashMap[Int, QualifiedNameKind]()

  val QualifiedName: QualifiedNameKind = new QualifiedNameKind(2, ".")

  abstract class NameKind(val tag: Int)

  class QualifiedNameKind(tag: Int, val separator: String)
  extends NameKind(tag) {
    qualifiedNameKinds(tag) = this
  }
}
