import scala.quoted.*

object SelectDynamicMacroImpl {
  def selectImpl[E: Type](
    ref: Expr[SQLSyntaxProvider[_]],
    name: Expr[String]
  )(using Quotes): Expr[SQLSyntax] = '{SQLSyntax("foo")}
}
