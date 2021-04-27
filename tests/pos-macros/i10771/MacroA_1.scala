import scala.quoted.*

case class MyQuoted(val ast: String, runtimeQuotes: List[String])

object MyQuoteMacro:
  inline def myquote: MyQuoted = ${ MyQuoteMacro.apply }
  def apply(using Quotes): Expr[MyQuoted] =
    '{ MyQuoted("p", ${Expr.ofList(List( '{ "foo" } ))}) }
