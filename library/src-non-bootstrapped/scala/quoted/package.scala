package scala

package object quoted {

  def run[T](expr: given QuoteContext => Expr[T]) given (toolbox: Toolbox): T =
    throw new Exception("Non bootsrapped library")

  def withQuoteContext[T](thunk: given QuoteContext => T) given (toolbox: Toolbox): T =
    throw new Exception("Non bootsrapped library")

  implicit object ExprOps {
    def (x: T) toExpr[T: Liftable] given QuoteContext: Expr[T] = the[Liftable[T]].toExpr(x)

    def (seq: Seq[Expr[T]]) toExprOfSeq[T] given (tp: Type[T], qctx: QuoteContext): Expr[Seq[T]] =
      throw new Exception("Non bootsrapped library")

    def (list: List[Expr[T]]) toExprOfList[T] given Type[T], QuoteContext: Expr[List[T]] =
      throw new Exception("Non bootsrapped library")
  }
}
