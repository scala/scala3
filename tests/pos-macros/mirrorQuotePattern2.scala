import scala.deriving._
import scala.quoted._

private def derivedExpr(x: Expr[Any])(using Quotes): Unit =
  x match
    case '{ type mtp1; ($m1 : Mirror.Sum { type MirroredElemTypes = mtp1 } & Mirror.Of[Any], $m2 : Mirror.Sum { type MirroredElemTypes = mtp2 } & Mirror.Of[Any]); ??? } =>
      val _: Expr[Mirror.Sum { type MirroredElemTypes = mtp1 } & Mirror.Of[Any]] = m1
      val _: Expr[Mirror.Sum { type MirroredElemTypes = mtp2 } & Mirror.Of[Any]] = m2
      '{ $m1: Mirror.Sum { type MirroredElemTypes = mtp1 } }
      '{ $m2: Mirror.Sum { type MirroredElemTypes = mtp2 } }
