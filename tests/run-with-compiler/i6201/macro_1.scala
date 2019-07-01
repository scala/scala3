import scala.quoted._
import scala.tasty._

inline def (inline x: String) strip: String =
  ${ stripImpl(x) }

def stripImpl(x: String) given (qctx: QuoteContext): Expr[String] =
  x.stripMargin.toExpr

inline def isHello(inline x: String): Boolean =
  ${ isHelloImpl(x) }

def isHelloImpl(x: String) given (qctx: QuoteContext): Expr[Boolean] =
  if (x == "hello") true.toExpr else false.toExpr

