import scala.compiletime.{erasedValue, summonFrom}
import scala.deriving._
import scala.quoted._

trait Lft[T]:
  def toExpr(x: T)(using Type[T], Quotes): Expr[T] // TODO remove `Type[T]`

object Lft {
  given Lft[Int] with
    def toExpr(x: Int)(using Type[Int], Quotes) = Expr(x)

  inline given derived[T](using inline m: Mirror.Of[T]): Lft[T] = ${ derivedExpr('m) }

  private def derivedExpr[T](mirrorExpr: Expr[Mirror.Of[T]])(using qctx: Quotes, tpe: Type[T]): Expr[Lft[T]] = {
    mirrorExpr match {
      case '{ $mirrorExpr : Mirror.Sum { type MirroredElemTypes = mirroredElemTypes } } =>
        val liftables = Expr.ofSeq(elemTypesLfts[mirroredElemTypes])
        '{ new LiftableSum[T, mirroredElemTypes]($mirrorExpr, $liftables) }
      case '{ $mirrorExpr : Mirror.Product { type MirroredElemTypes = mirroredElemTypes } } =>
        val liftableExprs = Expr.ofSeq(elemTypesLfts[mirroredElemTypes])
        '{ new LiftableProduct[T, mirroredElemTypes]($mirrorExpr, $liftableExprs) }
    }
  }

  class LiftableSum[T, MElemTypes](
    mirror: Mirror.Sum { type MirroredElemTypes = MElemTypes; type MirroredMonoType = T },
    liftables: Seq[Lft[_]] // TODO make Lft creation lazy
  ) extends Lft[T]:
      def toExpr(x: T)(using Type[T], Quotes): Expr[T] =
        val ordinal = mirror.ordinal(x)
        val tp = Expr.summon[Mirror.SumOf[T]].get match
          case '{ $mirrorExpr : Mirror.Sum { type MirroredElemTypes = mirroredElemTypes } } =>
            elemType[mirroredElemTypes](ordinal)
        val liftable = liftables.apply(ordinal).asInstanceOf[Lft[T]]
        liftable.toExpr(x)(using tp.asInstanceOf[Type[T]], summon[Quotes])
  end LiftableSum

  class LiftableProduct[T, MElemTypes](
    mirror: Mirror.Product { type MirroredElemTypes = MElemTypes; type MirroredMonoType = T },
    liftables: Seq[Lft[_]]
  ) extends Lft[T]:
      def toExpr(x: T)(using Type[T], Quotes): Expr[T] =
        val mirrorExpr = Expr.summon[Mirror.ProductOf[T]].get
        val elemExprs =
          x.asInstanceOf[Product].productIterator.zip(liftables.iterator).map { (elem, lift) =>
            lift.asInstanceOf[Lft[Any]].toExpr(elem)
          }.toSeq
        val elemsTupleExpr = Expr.ofTupleFromSeq(elemExprs)
        '{ $mirrorExpr.fromProduct($elemsTupleExpr) }
  end LiftableProduct

  private def elemTypesLfts[X: Type](using Quotes): List[Expr[Lft[_]]] =
    Type.of[X] match
      case '[ head *: tail ] =>
        Expr.summon[Lft[head]].getOrElse(quotes.reflect.report.throwError(s"Could not find given Lft[${Type.show[head]}]")) :: elemTypesLfts[tail]
      case '[ EmptyTuple ] => Nil

  private def elemType[X: Type](ordinal: Int)(using Quotes): Type[_] =
    Type.of[X] match
      case '[ head *: tail ] =>
        if ordinal == 0 then Type.of[head]
        else elemType[tail](ordinal - 1)
}
