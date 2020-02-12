import scala.compiletime.{erasedValue, summonFrom}
import scala.deriving._
import scala.quoted._
import scala.quoted.matching._


trait Lft[T] {
  def toExpr(x: T): (QuoteContext, Type[T]) ?=> Expr[T]
}

object Lft {
  given Lft[Int] {
    def toExpr(x: Int) = Expr(x)
  }

  inline def summonInline[T] = summonFrom { case x: T => x }

  inline def summonAll[T <: Tuple]: List[?] = inline erasedValue[T] match
    case _: Unit => Nil
    case _: (t *: ts) => summonInline[t] :: summonAll[ts]

  inline given derived[T](using inline m: Mirror.Of[T]) as Lft[T] =
    val liftables = summonAll[Tuple.Map[m.MirroredElemTypes, Lft]]
    inline m match
      case m: Mirror.SumOf[T] => derivedSum(m, liftables)
      case m: Mirror.ProductOf[T] => derivedProduct(m, liftables)

  private def derivedSum[T](m: Mirror.SumOf[T], liftables: List[_]): Lft[T] = {
    new Lft[T] {
      def toExpr(x: T): (QuoteContext, Type[T]) ?=> Expr[T] =
        liftables(m.ordinal(x)).asInstanceOf[Lft[T]].toExpr(x)
    }
  }

  private def derivedProduct[T](m: Mirror.ProductOf[T], liftables: List[_]): Lft[T] = {
    new Lft[T] {
      def toExpr(x: T): (QuoteContext, Type[T]) ?=> Expr[T] =
        val p = x.asInstanceOf[Product]
        val mirrorType = '[Mirror.ProductOf[T]]
        val me = summonExpr(using mirrorType).getOrElse(summon[QuoteContext].throwError(s"Could not find a `${mirrorType.show}`"))
        val elements = p.productIterator.zip(liftables).map { case (elem, liftable) => liftable.asInstanceOf[Lft[Any]].toExpr(elem) }.toSeq
        '{
          val elems = new ArgProduct(${Expr.ofSeq(elements)}: _*)
          val mirror = $me
          mirror.fromProduct(elems)
         }
    }
  }

  class ArgProduct[T](seq: T*) extends Product {
    def canEqual(that: Any): Boolean = false
    def productArity: Int = seq.size
    def productElement(n: Int): Any = seq(n)
  }

}
