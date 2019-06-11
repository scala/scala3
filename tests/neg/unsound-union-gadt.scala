object Test {
  enum Expr[+T] {
    case StrLit() extends Expr[String]
    case IntLit() extends Expr[Int]
  }
  import Expr._

  def foo[T](e: Expr[T]) = e match {
    case _: (StrLit | IntLit) =>
      val str: T = "" // error
      val int: T = 42 // error
  }
}
