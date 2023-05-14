import scala.quoted.*

inline def unswitch[T](inline x: T): (Int, Seq[T]) = ${ unswitchExpr('x) }

private def unswitchExpr[T: Type](x: Expr[T])(using Quotes): Expr[(Int, Seq[T])] =
  import quotes.reflect.*
  x.asTerm match
    case Inlined(_, _, Block(Nil, Match(scrut, cases))) =>
      val exprs: List[Expr[T]] = cases.zipWithIndex.map {
        case (CaseDef(Literal(IntConstant(i)), None, body), j) if i == j => body.asExprOf[T]
        case (cse, _) => report.errorAndAbort("unexpected case: ", cse.pos)
      }
      scrut.asExpr match
        case '{ $scrutExpr: Int } => '{ ($scrutExpr, ${Expr.ofSeq(exprs)}) }
        case _ => report.errorAndAbort("not Int scrutinee", scrut.pos)
    case _ => report.errorAndAbort("not a match", x)
