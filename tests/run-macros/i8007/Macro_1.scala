import scala.deriving._
import scala.quoted._


object Macro1 {

  def mirrorFields[T: Type](using qctx: QuoteContext): List[String] =
    Type[T] match {
      case '[field *: fields] => Type.show[field] :: mirrorFields[fields]
      case '[EmptyTuple] => Nil
    }

  // Demonstrates the use of quoted pattern matching
  // over a refined type extracting the tuple type
  // for e.g., MirroredElemLabels
  inline def test1[T](value: =>T): List[String] =
    ${ test1Impl('value) }

  def test1Impl[T: Type](value: Expr[T])(using qctx: QuoteContext): Expr[List[String]] = {
    import qctx.reflect._

    val mirrorTpe = Type[Mirror.Of[T]]

    Expr.summon(using mirrorTpe).get match {
      case '{ $m: Mirror.ProductOf[T]{ type MirroredElemLabels = elems } } => {
        Expr(mirrorFields[elems])
      }
    }
  }
}