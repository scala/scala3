import scala.quoted.*

object Macro {

  extension (inline sc: StringContext)
    inline def x(inline args: Int*): String = ${ code('sc, 'args) }

  def code(strCtxExpr: Expr[StringContext], argsExpr: Expr[Seq[Int]])(using Quotes): Expr[String] =
    var res: Expr[String] = null
    for _ <- 0 to 5_000 do
      (strCtxExpr, argsExpr) match {
        case ('{ StringContext(${Varargs(Exprs(parts))}*) }, Varargs(Exprs(args))) =>
          res = Expr(StringContext(parts*).s(args*))
        case _ => ???
      }
    res

}
