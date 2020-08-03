import scala.deriving._
import scala.quoted._


object Macro2 {

  def mirrorFields[T](t: Staged[T])(using qctx: QuoteContext): List[String] =
    t match {
      case '[$field *: $fields] => field.show.substring(1, field.show.length-1) :: mirrorFields(fields)
      case '[EmptyTuple] => Nil
    }

  trait JsonEncoder[T] {
    def encode(elem: T): String
  }

  object JsonEncoder {
    def emitJsonEncoder[T](body: T => String): JsonEncoder[T]=
      new JsonEncoder[T] {
          def encode(elem: T): String = body(elem)
        }

    def derived[T: Staged](ev: Expr[Mirror.Of[T]])(using qctx: QuoteContext): Expr[JsonEncoder[T]] = {
      import qctx.tasty._

      val fields = ev match {
        case '{ $m: Mirror.ProductOf[T] { type MirroredElemLabels = $t } } =>
          mirrorFields(t)
      }

      val body: Expr[T] => Expr[String] = elem =>
        fields.reverse.foldLeft(Expr("")){ (acc, field) =>
          val res = Select.unique(elem.unseal, field).seal
          '{ $res.toString + " " + $acc }
        }

      '{
        emitJsonEncoder((x: T) => ${body('x)})
      }
    }
  }

  inline def test2[T](value: =>T): Unit = ${ test2Impl('value) }

  def test2Impl[T: Staged](value: Expr[T])(using qctx: QuoteContext): Expr[Unit] = {
    import qctx.tasty._

    val mirrorTpe = '[Mirror.Of[T]]
    val mirrorExpr = Expr.summon(using mirrorTpe).get
    val derivedInstance = JsonEncoder.derived(mirrorExpr)

    '{
      val res = $derivedInstance.encode($value)
      println(res)
    }
  }
}