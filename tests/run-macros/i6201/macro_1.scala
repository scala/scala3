import scala.quoted._

extension (inline x: String) inline def strip: String =
  ${ stripImpl('x) }

def stripImpl(using s: Scope)(x: s.Expr[String]): s.Expr[String] =
  Expr(x.unliftOrError.stripMargin)

inline def isHello(inline x: String): Boolean =
  ${ isHelloImpl('x) }

def isHelloImpl(using s: Scope)(x: s.Expr[String]): s.Expr[Boolean] =
  if (x.unliftOrError == "hello") Expr(true) else Expr(false)
