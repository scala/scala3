import scala.deriving._
import scala.quoted._


object Macro2 {

  def mirrorFields[T](using s: Scope)(t: s.Type[T]): List[String] =
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

    def derived[T](using s: Scope)(ev: s.Expr[Mirror.Of[T]])(using s.Type[T]): s.Expr[JsonEncoder[T]] = {
      import s.tasty._

      val fields = ev match {
        case '{ $m: Mirror.ProductOf[T] { type MirroredElemLabels = $t } } =>
          mirrorFields(t)
      }

      def body(using s1: s.Nested): s1.Expr[T] => s1.Expr[String] = elem =>
        fields.reverse.foldLeft(Expr("")){ (acc, field) =>
          val res = Select.unique(elem, field).seal
          '{ $res.toString + " " + $acc }
        }

      '{
        emitJsonEncoder((x: T) => ${body('x)})
      }
    }
  }

  inline def test2[T](value: =>T): Unit = ${ test2Impl('value) }

  def test2Impl[T](using s: Scope)(value: s.Expr[T])(using s.Type[T]): s.Expr[Unit] = {
    import s.tasty._

    val mirrorExpr = Expr.summon[Mirror.Of[T]].get
    val derivedInstance = JsonEncoder.derived(mirrorExpr)

    '{
      val res = $derivedInstance.encode($value)
      println(res)
    }
  }
}