import scala.deriving._
import scala.quoted._


trait Eq[T] {
  def eqv(x: T, y: T): Boolean
}

object Eq {
  given Eq[String] {
    def eqv(x: String, y: String) = x == y
  }

  given Eq[Int] {
    def eqv(x: Int, y: Int) = x == y
  }

  def eqProduct[T](body: (T, T) => Boolean): Eq[T] =
    new Eq[T] {
      def eqv(x: T, y: T): Boolean = body(x, y)
    }

  def eqSum[T](body: (T, T) => Boolean): Eq[T] =
    new Eq[T] {
      def eqv(x: T, y: T): Boolean = body(x, y)
    }

  def summonAll[T](using t: Type[T])(using qctx: QuoteContext): List[Expr[Eq[_]]] = t match {
    case '[String *: tpes] => '{ summon[Eq[String]] }  :: summonAll[tpes]
    case '[Int *: tpes]    => '{ summon[Eq[Int]] }     :: summonAll[tpes]
    case '[tpe *: tpes]   => derived[tpe] :: summonAll[tpes]
    case '[EmptyTuple] => Nil
  }

  given derived[T: Type](using qctx: QuoteContext) as Expr[Eq[T]] = {
    import qctx.reflect._

    val ev: Expr[Mirror.Of[T]] = Expr.summon(using Type[Mirror.Of[T]]).get

    ev match {
      case '{ $m: Mirror.ProductOf[T] { type MirroredElemTypes = elementTypes }} =>
        val elemInstances = summonAll[elementTypes]
        val eqProductBody: (Expr[T], Expr[T]) => Expr[Boolean] = (x, y) => {
          elemInstances.zipWithIndex.foldLeft(Expr(true: Boolean)) {
            case (acc, (elem, index)) =>
              val e1 = '{$x.asInstanceOf[Product].productElement(${Expr(index)})}
              val e2 = '{$y.asInstanceOf[Product].productElement(${Expr(index)})}

              '{ $acc && $elem.asInstanceOf[Eq[Any]].eqv($e1, $e2) }
          }
        }
        '{
          eqProduct((x: T, y: T) => ${eqProductBody('x, 'y)})
        }

      case '{ $m: Mirror.SumOf[T] { type MirroredElemTypes = elementTypes }} =>
        val elemInstances = summonAll[elementTypes]
        val eqSumBody: (Expr[T], Expr[T]) => Expr[Boolean] = (x, y) => {
          val ordx = '{ $m.ordinal($x) }
          val ordy = '{ $m.ordinal($y) }

          val elements = Expr.ofList(elemInstances)
          '{
              $ordx == $ordy && $elements($ordx).asInstanceOf[Eq[Any]].eqv($x, $y)
          }
        }

        '{
          eqSum((x: T, y: T) => ${eqSumBody('x, 'y)})
        }
    }
  }
}

object Macro3 {
  extension [T](inline x: T) inline def === (inline y: T)(using eq: Eq[T]): Boolean = eq.eqv(x, y)

  implicit inline def eqGen[T]: Eq[T] = ${ Eq.derived[T] }
}
