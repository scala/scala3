import scala.quoted._


extension (sc: StringContext) inline def showMe(inline args: Any*): String = ${ showMeExpr('sc, 'args) }

private def showMeExpr(sc: Expr[StringContext], argsExpr: Expr[Seq[Any]])(using qctx: QuoteContext): Expr[String] = {
  argsExpr match {
    case Varargs(argExprs) =>
      val argShowedExprs = argExprs.map {
        case '{ $arg: tp } =>
          val showTp = Type[Show[tp]]
          Expr.summon(using showTp) match {
            case Some(showExpr) => '{ $showExpr.show($arg) }
            case None => report.error(s"could not find implicit for ${Type.show[Show[Tp]]}", arg); '{???}
          }
      }
      val newArgsExpr = Varargs(argShowedExprs)
      '{ $sc.s($newArgsExpr: _*) }
    case _ =>
      // `new StringContext(...).showMeExpr(args: _*)` not an explicit `showMeExpr"..."`
      report.error(s"Args must be explicit", argsExpr)
      '{???}
  }
}

trait Show[-T] {
  def show(x: T): String
}

given Show[Int] = x => s"Int($x)"
given Show[String] = x => s"Str($x)"
