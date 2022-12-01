import scala.quoted._

final class Quoted[A] {
  inline def f(): Any = ${ Quoted.fExpr[A]() }
}

object Quoted {
  def fExpr[A : Type]()(using Quotes): Expr[Any] = '{ () }
}
