// LazyRef
trait Txn[T <: Txn[T]]

trait Elem[T <: Txn[T]]

trait Obj[T <: Txn[T]] extends Elem[T]

trait Copy[In <: Txn[In], Out <: Txn[Out]] {
  def copyImpl[Repr[~ <: Txn[~]] <: Elem[~]](in: Repr[In]): Repr[Out]

  def apply[Repr[~ <: Txn[~]] <: Elem[~]](in: Repr[In]): Repr[Out] = {
    val out = copyImpl[Repr](in)
    (in, out) match {
      case (inObj: Obj[In], outObj: Obj[Out]) =>     // problem here
        println("copy the attributes")
      case _ =>
    }
    out
  }
}
