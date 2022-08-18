trait Expr[T]
  case class IntExpr() extends Expr[Int]

  def flag: Boolean = ???

  def foo[T](ev: Expr[T]): Int | T = ev match
    case IntExpr() =>
      if flag then
        val i: T = ???
        i
      else
        (??? : Int)
