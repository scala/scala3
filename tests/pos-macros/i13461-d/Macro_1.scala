import scala.quoted.*
opaque type MyOpaque = Int
object MyOpaque:
  val one: MyOpaque = 1
  transparent inline def apply(): MyOpaque = ${ applyMacro }
  private def applyMacro(using Quotes): Expr[MyOpaque] =
    import quotes.reflect.*
    '{ one }
