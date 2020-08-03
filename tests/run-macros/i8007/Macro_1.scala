import scala.deriving._
import scala.quoted._


object Macro1 {

  def mirrorFields[T](t: Staged[T])(using qctx: QuoteContext): List[String] =
    t match {
      case '[$field *: $fields] => field.show :: mirrorFields(fields)
      case '[EmptyTuple] => Nil
    }

  // Demonstrates the use of quoted pattern matching
  // over a refined type extracting the tuple type
  // for e.g., MirroredElemLabels
  inline def test1[T](value: =>T): List[String] =
    ${ test1Impl('value) }

  def test1Impl[T: Staged](value: Expr[T])(using qctx: QuoteContext): Expr[List[String]] = {
    import qctx.tasty._

    val mirrorTpe = '[Mirror.Of[T]]

    Expr.summon(using mirrorTpe).get match {
      case '{ $m: Mirror.ProductOf[T]{ type MirroredElemLabels = $t } } => {
        Expr(mirrorFields(t))
      }
    }
  }
}