package scala.quoted

import scala.quoted.show.SyntaxHighlight

class QuoteContext(reflection: tasty.Reflection) {

  def show[T](expr: Expr[T], syntaxHighlight: SyntaxHighlight): String = {
    import reflection._
    expr.unseal.show(syntaxHighlight)
  }

  def show[T](tpe: Type[T], syntaxHighlight: SyntaxHighlight): String = {
    import reflection._
    tpe.unseal.show(syntaxHighlight)
  }

}
