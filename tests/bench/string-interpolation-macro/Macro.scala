import scala.quoted._

object Macro {

  extension (inline sc: StringContext)
    inline def x(inline args: Int*): String = ${ code('sc, 'args) }

  def code(strCtxExpr: Expr[StringContext], argsExpr: Expr[Seq[Int]])(using Quotes): Expr[String] =
    var res: Expr[String] = null
    for _ <- 0 to 5_000 do
      (strCtxExpr, argsExpr) match {
        case ('{ StringContext(${Varargs(Consts(parts))}: _*) }, Varargs(Consts(args))) =>
          res = Expr(StringContext(parts: _*).s(args: _*))
        case _ => ???
      }
    res

}
