object Test {
  sealed trait Expr[+T]
  case class IntVal[+T <: Int]() extends Expr[T]
  case object ActualInt extends Expr[Int]
  def eval[T](e: Expr[T]): T = e match {
    case IntVal() => 0 // error
    case ActualInt => 0
  }
}

