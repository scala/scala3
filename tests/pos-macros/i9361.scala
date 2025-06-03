// https://github.com/scala/scala3/issues/9361

import scala.quoted._

trait CPM[F[_]]

def matchTerm(t: Expr[Any])(using qctx: Quotes): Unit =
  t match
    case '{ ??? : CPM[m2] } => ???
