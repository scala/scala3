trait Txn[T <: Txn[T]]

trait Elem[T <: Txn[T]]

sealed trait State[+T]
final case class Done[T <: Txn[T]](elem: Elem[T]) extends State[T]
case object Busy extends State[Nothing]

trait Test[Out <: Txn[Out]] {
  def apply(opt: Option[State[Out]]): Any = opt match {
    case Some(state) =>
      state match {
        case Done(out)  => "foo"    // problem here
        case Busy       => throw new IllegalStateException("Cyclic object graph")
      }

    case None => "bar"
  }
}
