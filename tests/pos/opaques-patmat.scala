object OpaquePatmat {
  opaque type Pos = Int
  object Pos {
    def mkPos(i: Int): Pos = {
      require(i > 0)
      i
    }
    def coerce[F[_]](fa: F[Int]): F[Pos] = fa
  }
  
  sealed trait Expr[T]
  final case class PosExpr(p: Pos) extends Expr[Pos]
  final case class IntExpr(i: Int) extends Expr[Int]
  final case class StrExpr(s: String) extends Expr[String]
  
  def eval(e: Expr[Pos]): Pos = e match {
    case PosExpr(p) => p
    // both of the patterns need to be well-typed here,
    // since Pos is potentially equal to any other type
    case IntExpr(_) => Pos.mkPos(1)
    case StrExpr(_) => ???
  }
  
  eval(Pos.coerce[Expr](IntExpr(-1)))
}
