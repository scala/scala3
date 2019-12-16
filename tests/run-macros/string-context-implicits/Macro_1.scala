import scala.quoted._
import scala.quoted.matching._

inline def (sc: StringContext) showMe(args: =>Any*): String = ${ showMeExpr('sc, 'args) }

private def showMeExpr(sc: Expr[StringContext], argsExpr: Expr[Seq[Any]])(given qctx: QuoteContext): Expr[String] = {
  argsExpr match {
    case ExprSeq(argExprs) =>
      val argShowedExprs = argExprs.map {
        case '{ $arg: $tp } =>
          val showTp = '[Show[$tp]]
          summonExpr(given showTp) match {
            case Some(showExpr) => '{ $showExpr.show($arg) }
            case None => qctx.error(s"could not find implicit for ${showTp.show}", arg); '{???}
          }
      }
      val newArgsExpr = Expr.ofSeq(argShowedExprs)
      '{ $sc.s($newArgsExpr: _*) }
    case _ =>
      // `new StringContext(...).showMeExpr(args: _*)` not an explicit `showMeExpr"..."`
      qctx.error(s"Args must be explicit", argsExpr)
      '{???}
  }
}

trait Show[-T] {
  def show(x: T): String
}

given Show[Int] = x => s"Int($x)"
given Show[String] = x => s"Str($x)"
