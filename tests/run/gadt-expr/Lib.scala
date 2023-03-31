class Lib:
  def extract[A](expr: Expr[A]): A = (expr: @unchecked) match
    case IntExpr() =>
      val res: A = 1
      res
