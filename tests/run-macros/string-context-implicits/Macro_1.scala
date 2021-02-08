import scala.quoted.*


extension (sc: StringContext) inline def showMe(inline args: Any*): String = ${ showMeExpr('sc, 'args) }

private def showMeExpr(sc: Expr[StringContext], argsExpr: Expr[Seq[Any]])(using Quotes): Expr[String] =
  import quotes.reflect.report
  argsExpr match
    case Varargs(argExprs) =>
      val argShowedExprs = argExprs.map {
        case '{ $arg: tp } =>
          Expr.summon[Show[tp]] match
            case Some(showExpr) =>
              '{ $showExpr.show($arg) }
            case None =>
              report.error(s"could not find implicit for ${Type.show[Show[tp]]}", arg); '{???}
      }
      val newArgsExpr = Varargs(argShowedExprs)
      '{ $sc.s($newArgsExpr*) }
    case _ =>
      // `new StringContext(...).showMeExpr(args*)` not an explicit `showMeExpr"..."`
      report.error(s"Args must be explicit", argsExpr)
      '{ ??? }

trait Show[-T] {
  def show(x: T): String
}

object Show:
  given Show[Int] = x => s"Int($x)"
  given Show[String] = x => s"Str($x)"
