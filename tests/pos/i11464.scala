trait Txn[T <: Txn[T]]

trait Adjunct

trait Type0
trait Type[A1, Repr[~ <: Txn[~]] <: Expr[~, A1]] extends Type0

object Expr {
  def test(peer: Type0): Adjunct = {
    new AdjunctImpl(peer.asInstanceOf[Type[Any, ({ type R[~ <: Txn[~]] <: Expr[~, Any] }) # R]])
  }
}

trait Expr[T <: Txn[T], +A]

class AdjunctImpl[A, E[~ <: Txn[~]] <: Expr[~, A]](tpe: Type[A, E]) extends Adjunct
