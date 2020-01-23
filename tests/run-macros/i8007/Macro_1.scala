import scala.deriving._
import scala.quoted._
import scala.quoted.matching._

object Macro1 {

  def mirrorFields[T](t: Type[T])(given qctx: QuoteContext): List[String] =
    t match {
      case '[$field *: $fields] => field.show :: mirrorFields(fields)
      case '[Unit] => Nil
    }

  // Demonstrates the use of quoted pattern matching
  // over a refined type extracting the tuple type
  // for e.g., MirroredElemLabels
  inline def test1[T](value: =>T): List[String] =
    ${ test1Impl('value) }

  def test1Impl[T: Type](value: Expr[T])(given qctx: QuoteContext): Expr[List[String]] = {
    import qctx.tasty.{_, given}

    val mirrorTpe = '[Mirror.Of[T]]

    summonExpr(given mirrorTpe).get match {
      case '{ $m: Mirror.ProductOf[T]{ type MirroredElemLabels = $t } } => {
        Expr(mirrorFields(t))
      }
    }
  }
}