import scala.deriving._
import scala.quoted._


object Macro2 {

  def mirrorFields[T](using t: Type[T])(using qctx: QuoteContext): List[String] =
    t match {
      case '[field *: fields] => Type.show[field].substring(1, Type.show[field].length-1) :: mirrorFields[fields]
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

    def derived[T: Type](ev: Expr[Mirror.Of[T]])(using qctx: QuoteContext): Expr[JsonEncoder[T]] = {
      import qctx.reflect._

      val fields = ev match {
        case '{ $m: Mirror.ProductOf[T] { type MirroredElemLabels = labels } } =>
          mirrorFields[labels]
      }

      val body: Expr[T] => Expr[String] = elem =>
        fields.reverse.foldLeft(Expr("")){ (acc, field) =>
          val res = Select.unique(Term.of(elem), field).asExpr
          '{ $res.toString + " " + $acc }
        }

      '{
        emitJsonEncoder((x: T) => ${body('x)})
      }
    }
  }

  inline def test2[T](value: =>T): Unit = ${ test2Impl('value) }

  def test2Impl[T: Type](value: Expr[T])(using qctx: QuoteContext): Expr[Unit] = {
    import qctx.reflect._

    val mirrorTpe = Type.of[Mirror.Of[T]]
    val mirrorExpr = Expr.summon(using mirrorTpe).get
    val derivedInstance = JsonEncoder.derived(mirrorExpr)

    '{
      val res = $derivedInstance.encode($value)
      println(res)
    }
  }
}