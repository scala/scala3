
import scala.quoted.{Expr, QuoteContext}

object Inspect {
  inline def inspect[T <: AnyKind]: String = ${ inspectTpe[T] }

  def inspectTpe[T <: AnyKind](using tpe: quoted.Staged[T], qctx0: QuoteContext): Expr[String] = {
    val tree = summon[quoted.Staged[T]].unseal.tpe.typeSymbol.tree
    Expr(tree.show)
  }
}
