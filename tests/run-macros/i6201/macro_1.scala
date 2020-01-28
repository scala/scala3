import scala.quoted._

inline def (inline x: String) strip: String =
  ${ stripImpl('x) }

def stripImpl(x: Expr[String]) with (qctx: QuoteContext) : Expr[String] =
  Expr(x.value.stripMargin)

inline def isHello(inline x: String): Boolean =
  ${ isHelloImpl('x) }

def isHelloImpl(x: Expr[String]) with (qctx: QuoteContext) : Expr[Boolean] =
  if (x.value == "hello") Expr(true) else Expr(false)
