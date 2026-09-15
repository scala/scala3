import language.experimental.captureChecking

def value(
  quotes: scala.quoted.Quotes,
  expr: scala.quoted.Expr[Int]
)(using scala.quoted.FromExpr[Int]) =
  given scala.quoted.Quotes = quotes
  summon[scala.quoted.FromExpr[Int]].unapply(expr)
