
import scala.quoted.{Expr, QuoteContext}

object Inspect {
  inline def inspect[T <: AnyKind]: String = ${ inspectTpe[T] }

  def inspectTpe[T <: AnyKind](using tpe: quoted.Type[T], qctx0: QuoteContext): Expr[String] = {
    val tree = summon[quoted.Type[T]].asTypeTree.tpe.typeSymbol.tree
    Expr(tree.show)
  }
}
