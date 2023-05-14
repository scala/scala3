import scala.deriving.*
import scala.quoted.*


trait Eq[T] {
  def eqv(x: T, y: T): Boolean
}

object Eq {
  given Eq[String] with {
    def eqv(x: String, y: String) = x == y
  }

  given Eq[Int] with {
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

  def summonAll[T: Type](using Quotes): List[Expr[Eq[_]]] = Type.of[T] match {
    case '[String *: tpes] => '{ summon[Eq[String]] }  :: summonAll[tpes]
    case '[Int *: tpes]    => '{ summon[Eq[Int]] }     :: summonAll[tpes]
    case '[tpe *: tpes]   => derived[tpe] :: summonAll[tpes]
    case '[EmptyTuple] => Nil
  }

  given derived[T: Type](using q: Quotes): Expr[Eq[T]] = {
    import quotes.reflect.*

    val ev: Expr[Mirror.Of[T]] = Expr.summon(using Type.of[Mirror.Of[T]]).get

    ev match {
      case '{ $m: Mirror.ProductOf[T] { type MirroredElemTypes = elementTypes }} =>
        val elemInstances = summonAll[elementTypes]
        def eqProductBody(x: Expr[Product], y: Expr[Product])(using Quotes): Expr[Boolean] = {
          elemInstances.zipWithIndex.foldLeft(Expr(true)) {
            case (acc, ('{ $elem: Eq[t]}, index)) =>
              val indexExpr = Expr(index)
              val e1 = '{ $x.productElement($indexExpr).asInstanceOf[t] }
              val e2 = '{ $y.productElement($indexExpr).asInstanceOf[t] }
              '{ $acc && $elem.eqv($e1, $e2) }
          }
        }
        '{
          eqProduct((x: T, y: T) => ${eqProductBody('x.asExprOf[Product], 'y.asExprOf[Product])})
        }

      case '{ $m: Mirror.SumOf[T] { type MirroredElemTypes = elementTypes }} =>
        val elemInstances = summonAll[elementTypes]
        def eqSumBody(x: Expr[T], y: Expr[T])(using Quotes): Expr[Boolean] = {
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
