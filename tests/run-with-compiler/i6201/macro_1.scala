import scala.quoted._

inline def (inline x: String) strip: String =
  ${ stripImpl(x) }

def stripImpl(x: String)(given qctx: QuoteContext): Expr[String] =
  Expr(x.stripMargin)

inline def isHello(inline x: String): Boolean =
  ${ isHelloImpl(x) }

def isHelloImpl(x: String)(given qctx: QuoteContext): Expr[Boolean] =
  if (x == "hello") Expr(true) else Expr(false)

