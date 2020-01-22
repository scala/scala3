import scala.deriving._
import scala.quoted._
import scala.quoted.matching._
import scala.compiletime.{erasedValue, summonFrom, constValue}

object Macro3 {

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

    def summonAll[T](t: Type[T])(given qctx: QuoteContext): List[Expr[Eq[_]]] = t match {
      case '[$tpe *: $tpes] => summonExpr(given '[Eq[$tpe]]).get :: summonAll(tpes)
      case '[Unit] => Nil
    }

    def derived[T: Type](ev: Expr[Mirror.Of[T]])(given qctx: QuoteContext): Expr[Eq[T]] = {
      import qctx.tasty.{_, given}

      val elementTypes = ev match {
        case '{ $m: Mirror.ProductOf[T] { type MirroredElemTypes = $elem } } => elem
      }

      val elemInstances = summonAll(elementTypes)

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
    }
  }

  inline def test3[T](value: =>T, value2: =>T): Boolean = ${ test3Impl('value, 'value2) }

  def test3Impl[T: Type](value: Expr[T], value2: Expr[T])(given qctx: QuoteContext): Expr[Boolean] = {
    import qctx.tasty.{_, given}

    val mirrorTpe = '[Mirror.Of[T]]
    val mirrorExpr = summonExpr(given mirrorTpe).get
    val derivedInstance = Eq.derived(mirrorExpr)

    '{
      $derivedInstance.eqv($value, $value2)
    }
  }
}