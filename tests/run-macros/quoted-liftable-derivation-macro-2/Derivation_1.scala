import scala.compiletime.{erasedValue, summonFrom}
import scala.deriving._
import scala.quoted._

trait Lft[T]:
  def toExpr(x: T)(using Quotes): Expr[T]

object Lft {
  given Lft[Int] with
    def toExpr(x: Int)(using Quotes) = Expr(x)

  inline given derived: [T] => (inline m: Mirror.Of[T]) => Lft[T] = ${ derivedExpr('m) }

  private def derivedExpr[T](mirrorExpr: Expr[Mirror.Of[T]])(using Quotes, Type[T]): Expr[Lft[T]] = {
    val tpe = summonExprOrError[Type[T]]
    mirrorExpr match {
      case '{ $mirrorExpr : Mirror.Sum { type MirroredElemTypes = mirroredElemTypes } } =>
        val liftables = elemTypesLfts[mirroredElemTypes]
        val liftablesFn = '{ (x: Int) => ${ switchExpr('x, liftables) } }
        '{ new LiftableSum[T, mirroredElemTypes]($mirrorExpr, $liftablesFn)(using $tpe) }
      case '{ $mirrorExpr : Mirror.Product { type MirroredElemTypes = mirroredElemTypes } } =>
        val liftableExprs = Expr.ofSeq(elemTypesLfts[mirroredElemTypes])
        '{ new LiftableProduct[T, mirroredElemTypes]($mirrorExpr, $liftableExprs)(using $tpe) }
    }
  }

  class LiftableSum[T, MElemTypes](
    mirror: Mirror.Sum { type MirroredElemTypes = MElemTypes; type MirroredMonoType = T },
    liftables: Int => Lft[_]
  )(using Type[T]) extends Lft[T]:
      def toExpr(x: T)(using Quotes): Expr[T] =
        val ordinal = mirror.ordinal(x)
        val liftable = liftables.apply(ordinal).asInstanceOf[Lft[T]]
        liftable.toExpr(x)
  end LiftableSum

  class LiftableProduct[T, MElemTypes](
    mirror: Mirror.Product { type MirroredElemTypes = MElemTypes; type MirroredMonoType = T },
    liftables: Seq[Lft[_]]
  )(using Type[T]) extends Lft[T]:
      def toExpr(x: T)(using Quotes): Expr[T] =
        val mirrorExpr = summonExprOrError[Mirror.ProductOf[T]]
        val xProduct = x.asInstanceOf[Product]
        val anyLiftables = liftables.asInstanceOf[Seq[Lft[Any]]]
        val elemExprs =
          xProduct.productIterator.zip(anyLiftables.iterator).map {
            (elem, lift) => lift.toExpr(elem)
          }.toSeq
        val elemsTupleExpr = Expr.ofTupleFromSeq(elemExprs)
        '{ $mirrorExpr.fromProduct($elemsTupleExpr) }
  end LiftableProduct

  private def elemTypesLfts[X: Type](using Quotes): List[Expr[Lft[_]]] =
    Type.of[X] match
      case '[ head *: tail ] => summonExprOrError[Lft[head]] :: elemTypesLfts[tail]
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
        quotes.reflect.report.errorAndAbort(s"Could not find implicit ${Type.show[T]}")

  private def switchExpr(scrutinee: Expr[Int], seq: List[Expr[Lft[_]]])(using Quotes): Expr[Lft[_]] =
    import quotes.reflect._
    val cases = seq.zipWithIndex.map {
      (expr, i) => CaseDef(Literal(IntConstant(i)), None, expr.asTerm)
    }
    Match(scrutinee.asTerm, cases).asExprOf[Lft[_]]

}
