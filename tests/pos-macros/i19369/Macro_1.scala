import scala.quoted._

object Unapplier:
  transparent inline def unapply(arg: Any): Option[Int] = ${unapplyImpl('arg)}

  def unapplyImpl(using Quotes)(argExpr: Expr[Any]): Expr[Option[Int]] =
    import quotes.reflect._
    // The code below will recurse, throwing cyclic reference, if it is being inlined too early,
    // which is caused by the same problem as the duplicate macro execution here
    println(Symbol.spliceOwner.owner.tree)
    '{Some(1)}
