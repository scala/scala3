trait Txn[T <: Txn[T]]

sealed trait SkipList[T <: Txn[T]]

trait Set[T <: Txn[T]] extends SkipList[T]

object HASkipList {
  def debug[T <: Txn[T]](in: SkipList[T]): Set[T] = in match {
    case impl: Set[T] => impl
    // case _ =>
  }
}
