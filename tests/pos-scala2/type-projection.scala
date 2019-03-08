trait Txn[S <: Sys[S]] {
  def system: S

  def newId(): S#Id

  def newVar[A](id: S#Id, init: A): S#Vr[A]
}

trait Var[Tx, A] {
  def apply()(implicit tx: Tx): A

  def update(x: A)(implicit tx: Tx): Unit
}

trait Sys[S <: Sys[S]] {
  type Tx <: Txn[S]
  type Id
  type Vr[A] <: Var[S#Tx, A]
}

abstract class UseCase[S <: Sys[S]](id: S#Id) {
  def mk(x: Int)(implicit tx: S#Tx): S#Vr[Int] = {
    val vr = tx.newVar[Int](id, x)
    vr
  }

  def rd(vr: S#Vr[Int])(implicit tx: S#Tx): Int = vr()
}
