trait Txn[T <: Txn[T]]

object Impl {
  sealed trait Entry[T <: Txn[T], A]
  case class EntrySingle[T <: Txn[T], A](term: Long, v: A)  extends Entry[T, A]
}

trait Impl[T <: Txn[T], K] {
  import Impl.*

  def put[A](): Unit = {
    val opt: Option[Entry[T, A]] = ???

    opt match {
      case Some(EntrySingle(_, prevValue)) => ???   // crashes
      case _ =>
    }
  }
}