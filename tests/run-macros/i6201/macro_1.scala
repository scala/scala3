import scala.quoted.*

extension (inline x: String) inline def strip: String =
  ${ stripImpl('x) }

def stripImpl(x: Expr[String])(using Quotes) : Expr[String] =
  Expr(x.valueOrError.stripMargin)

inline def isHello(inline x: String): Boolean =
  ${ isHelloImpl('x) }

def isHelloImpl(x: Expr[String])(using Quotes) : Expr[Boolean] =
  if (x.valueOrError == "hello") Expr(true) else Expr(false)
