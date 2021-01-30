import scala.quoted.*

case class MyQuoted(val ast: String, sub: String)

object MyQuoteMacro {
  inline def myquote(inline content: MyContent): MyQuoted = ${ MyQuoteMacro.apply('content) }
  def apply(content: Expr[MyContent])(using Quotes): Expr[MyQuoted] = {
    import quotes.reflect.*
    '{ MyQuoted($content.key, null) }
  }
}
