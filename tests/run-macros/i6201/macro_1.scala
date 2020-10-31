import scala.quoted._

object M:

  extension (inline x: String) inline def strip: String =
    ${ stripImpl('x) }

  def stripImpl(x: Expr[String])(using qctx: QuoteContext) : Expr[String] =
    Expr(x.unliftOrError.stripMargin)

  inline def isHello(inline x: String): Boolean =
    ${ isHelloImpl('x) }

  def isHelloImpl(x: Expr[String])(using qctx: QuoteContext) : Expr[Boolean] =
    if (x.unliftOrError == "hello") Expr(true) else Expr(false)
