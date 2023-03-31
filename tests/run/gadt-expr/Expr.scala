sealed trait Expr[A]
final case class  IntExpr() extends Expr[Int]
final case class BoolExpr() extends Expr[Boolean]
