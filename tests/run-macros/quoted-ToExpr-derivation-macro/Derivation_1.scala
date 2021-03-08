import scala.compiletime.{erasedValue, summonFrom}
import scala.deriving._
import scala.quoted._

object ToExprMaker {

  inline given derived[T](using inline m: Mirror.Of[T]): ToExpr[T] = ${ derivedExpr('m) }

  private def derivedExpr[T](mirrorExpr: Expr[Mirror.Of[T]])(using Quotes, Type[T]): Expr[ToExpr[T]] = {
    val tpe = summonExprOrError[Type[T]]
    mirrorExpr match {
      case '{ $mirrorExpr : Mirror.Sum { type MirroredElemTypes = mirroredElemTypes } } =>
        val liftables = elemTypesToExprs[mirroredElemTypes]
        val liftablesFn = '{ (x: Int) => ${ switchExpr('x, liftables) } }
        '{ new SumToExpr[T, mirroredElemTypes]($mirrorExpr, $liftablesFn)(using $tpe) }
      case '{ $mirrorExpr : Mirror.Product { type MirroredElemTypes = mirroredElemTypes } } =>
        val liftableExprs = Expr.ofSeq(elemTypesToExprs[mirroredElemTypes])
        '{ new ProductToExpr[T, mirroredElemTypes]($mirrorExpr, $liftableExprs)(using $tpe) }
    }
  }

  // TODO hide from users
  class SumToExpr[T, MElemTypes](
    mirror: Mirror.Sum { type MirroredElemTypes = MElemTypes; type MirroredMonoType = T },
    liftables: Int => ToExpr[_]
  )(using Type[T]) extends ToExpr[T]:
      def apply(x: T)(using Quotes): Expr[T] =
        val ordinal = mirror.ordinal(x)
        val liftable = liftables.apply(ordinal).asInstanceOf[ToExpr[T]]
        liftable.apply(x)
  end SumToExpr

  // TODO hide from users
  class ProductToExpr[T, MElemTypes](
    mirror: Mirror.Product { type MirroredElemTypes = MElemTypes; type MirroredMonoType = T },
    liftables: Seq[ToExpr[_]]
  )(using Type[T]) extends ToExpr[T]:
      def apply(x: T)(using Quotes): Expr[T] =
        val mirrorExpr = summonExprOrError[Mirror.ProductOf[T]]
        val xProduct = x.asInstanceOf[Product]
        val anyToExprs = liftables.asInstanceOf[Seq[ToExpr[Any]]]
        val elemExprs =
          xProduct.productIterator.zip(anyToExprs.iterator).map {
            (elem, lift) => lift(elem)
          }.toSeq
        val elemsTupleExpr = Expr.ofTupleFromSeq(elemExprs)
        '{ $mirrorExpr.fromProduct($elemsTupleExpr) }
  end ProductToExpr

  private def elemTypesToExprs[X: Type](using Quotes): List[Expr[ToExpr[_]]] =
    Type.of[X] match
      case '[ head *: tail ] => summonExprOrError[ToExpr[head]] :: elemTypesToExprs[tail]
      case '[ EmptyTuple ] => Nil

  private def elemType[X: Type](ordinal: Int)(using Quotes): Type[_] =
    Type.of[X] match
      case '[ head *: tail ] =>
        if ordinal == 0 then Type.of[head]
        else elemType[tail](ordinal - 1)

  private def summonExprOrError[T: Type](using Quotes): Expr[T] =
    Expr.summon[T] match
      case Some(expr) => expr
      case None =>
        quotes.reflect.report.throwError(s"Could not find implicit ${Type.show[T]}")

  private def switchExpr(scrutinee: Expr[Int], seq: List[Expr[ToExpr[_]]])(using Quotes): Expr[ToExpr[_]] =
    import quotes.reflect._
    val cases = seq.zipWithIndex.map {
      (expr, i) => CaseDef(Literal(IntConstant(i)), None, expr.asTerm)
    }
    Match(scrutinee.asTerm, cases).asExprOf[ToExpr[_]]

}
