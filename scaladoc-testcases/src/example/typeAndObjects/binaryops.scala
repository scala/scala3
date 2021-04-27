package example.typeAndObjects


sealed trait Expr

object Expr{
  case class BinaryOp(offset: Int, lhs: Expr, op: BinaryOp.Op, rhs: Expr) extends Expr
  
  object BinaryOp{
    sealed trait Op
    case object `<<` extends Op
    case object `>>` extends Op
  }
}