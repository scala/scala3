import scala.quoted._

object Unapplier:
  transparent inline def unapply(arg: Any): Option[Int] = ${unapplyImpl('arg)}

  def unapplyImpl(using Quotes)(argExpr: Expr[Any]): Expr[Option[Int]] =
    executionCount += 1
    assert(executionCount == 1, "macro should only expand once")
    '{Some(1)}

  private var executionCount = 0
