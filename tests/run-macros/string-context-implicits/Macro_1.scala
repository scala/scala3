import scala.quoted._


extension (sc: StringContext) inline def showMe(inline args: Any*): String = ${ showMeExpr('sc, 'args) }

private def showMeExpr(using s: Scope)(sc: s.Expr[StringContext], argsExpr: s.Expr[Seq[Any]]): s.Expr[String] = {
  argsExpr match {
    case Varargs(argExprs) =>
      val argShowedExprs = argExprs.map[s.Expr[Any]] {
        case '{ $arg: $tp } =>
          val showTp = '[Show[$tp]]
          Expr.summon[Show[$tp]] match {
            case Some(showExpr) => '{ $showExpr.show($arg) }
            case None => report.errorOn(arg, s"could not find implicit for ${showTp.show}"); '{???}
          }
      }
      val newArgsExpr = Varargs(argShowedExprs)
      '{ $sc.s($newArgsExpr: _*) }
    case _ =>
      // `new StringContext(...).showMeExpr(args: _*)` not an explicit `showMeExpr"..."`
      report.errorOn(argsExpr, s"Args must be explicit")
      '{???}
  }
}

trait Show[-T] {
  def show(x: T): String
}

given Show[Int] = x => s"Int($x)"
given Show[String] = x => s"Str($x)"
