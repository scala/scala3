object O {
   sealed trait Expr[T]
   case class IExpr(x: Int) extends Expr[Int]
   case class BExpr(b: Boolean) extends Expr[Boolean]

   def foo[T](x: Expr[T], y: Expr[T]) = (x, y) match {
      case (IExpr(_), IExpr(_)) => true
      case (BExpr(_), BExpr(_)) => false
   }
}