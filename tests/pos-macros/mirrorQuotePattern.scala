//> using options -experimental

import scala.deriving._
import scala.quoted._

private def derivedExpr[T](mirrorExpr: Expr[Mirror.Of[T]])(using Quotes, Type[T]): Expr[Any] = {
  mirrorExpr match {
    case '{ $mirrorExpr : Mirror.Sum { type MirroredElemTypes = mirroredElemTypes } } =>
      '{ liftableSum[mirroredElemTypes]($mirrorExpr) }
    case '{ type mirroredElemTypes; $mirrorExpr : Mirror.Sum { type MirroredElemTypes = mirroredElemTypes } } =>
      '{ liftableSum[mirroredElemTypes]($mirrorExpr) }
  }
}

def liftableSum[MElemTypes](mirror: Mirror.Sum { type MirroredElemTypes = MElemTypes }): Any = ???
