
import scala.quoted._

@deprecated object A

object lib:
  inline def m() = ${mImpl}

  def mImpl(using Quotes): Expr[Any] =
    import quotes.reflect._
    Ref(Symbol.classSymbol("A$").companionModule).asExpr
