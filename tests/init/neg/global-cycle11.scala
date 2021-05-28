import scala.collection.mutable

object NameKinds {
  private val qualifiedNameKinds = mutable.HashMap[Int, QualifiedNameKind]()

  val QualifiedName: QualifiedNameKind = new QualifiedNameKind(2, ".")

  abstract class NameKind(val tag: Int)

  class QualifiedNameKind(tag: Int, val separator: String)
  extends NameKind(tag) {
    qualifiedNameKinds(tag) = this
  }
}

object A {              // error
  val n: Int = B.m
}

object B {
  val m: Int = A.n
}
