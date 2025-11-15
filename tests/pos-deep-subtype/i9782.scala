//> using options -Werror

trait Txn[T <: Txn[T]]

trait Elem[T <: Txn[T]]

trait Obj[T <: Txn[T]] extends Elem[T]

class Test {

  def apply[Repr[~ <: Txn[~]] <: Elem[~], In <: Txn[In]](in: Repr[In]): Unit = {
    in match {
      case inObj: Obj[In] =>     // problem here
      case _ =>
    }
  }
}
