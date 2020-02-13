import scala.compiletime.{erasedValue, summonFrom}
import scala.deriving._
import scala.quoted._
import scala.quoted.matching._


trait Lft[T]:
  def toExpr(x: T): QuoteContext ?=> Expr[T]

object Lft {
  given Lft[Int]:
    def toExpr(x: Int) = Expr(x)

  inline given derived[T](using inline m: Mirror.Of[T]) as Lft[T] = ${ derivedExpr('m) }

  private def derivedExpr[T](mirrorExpr: Expr[Mirror.Of[T]])(using qctx: QuoteContext, tpe: Type[T]): Expr[Lft[T]] = {
    def elemTypesLfts(tp: Type[_]): List[Expr[Lft[_]]] =
      tp match
        case '[ $head *: $tail ] =>
          summonExpr(using '[Lft[$head]]).getOrElse(qctx.throwError(s"Could not find given Lft[${head.show}]")) :: elemTypesLfts(tail)
        case '[ Unit ] => Nil

    mirrorExpr match {
      case '{ $mirrorExpr : Mirror.Sum { type MirroredElemTypes = $mirroredElemTypes } } =>
        val liftableExprs = elemTypesLfts(mirroredElemTypes)
        '{
          new Lft[T]:
            def toExpr(x: T) =
              val mirror = $mirrorExpr
              val liftable = ${Expr.ofSeq(liftableExprs)}.apply(mirror.ordinal(x)).asInstanceOf[Lft[T]] // TODO generate switch
              liftable.toExpr(x)
        }

      case '{ $mirrorExpr : Mirror.Product { type MirroredElemTypes = $mirroredElemTypes } } =>
        val liftableExprs = Expr.ofList(elemTypesLfts(mirroredElemTypes))
        '{
          new Lft[T]:
            def toExpr(x: T) =
              val liftables = $liftableExprs
              val lifted = Expr.ofSeq(liftables.zipWithIndex.map { (liftable, i) =>
                liftable.asInstanceOf[Lft[AnyRef]].toExpr(productElement(x, i))
              })
              val liftedProduct = '{ new ArrayProduct(Array[AnyRef]($lifted: _*)) }
              val mirror = summonExpr(using '[Mirror.ProductOf[T]]).get
              '{ $mirror.fromProduct($liftedProduct) }
        }
    }
  }

}
