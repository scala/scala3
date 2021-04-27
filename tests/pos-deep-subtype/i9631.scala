trait Txn[T <: Txn[T]]

object SkipList {
  trait Set[T <: Txn[T], A] extends SkipList[T, A, A]
}
sealed trait SkipList[T <: Txn[T], A, E]

object HASkipList {
  def debug[T <: Txn[T], A](in: SkipList[T, A, _], key: A)(implicit tx: T): Int = in match {
    case impl: Impl[T, A, _] => impl.foo(key)
    case _ => -1
  }

  private trait Impl[T <: Txn[T], A, E] {
    self: SkipList[T, A, E] =>

    def foo(key: A)(implicit tx: T): Int
  }
}
