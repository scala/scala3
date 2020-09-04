trait Txn[T <: Txn[T]]

sealed trait SkipList[T <: Txn[T]]

trait Set[T <: Txn[T]] extends SkipList[T]

object HASkipList {
  def debug[T <: Txn[T]](in: SkipList[T]): Unit = in match {
    case impl: Set[T] =>
    case _ =>
  }
}
