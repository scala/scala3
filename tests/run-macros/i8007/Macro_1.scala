import scala.deriving._
import scala.quoted._


object Macro1 {

  def mirrorFields[T](using s: Scope)(t: s.Type[T]): List[String] =
    t match {
      case '[$field *: $fields] => field.show :: mirrorFields(fields)
      case '[EmptyTuple] => Nil
    }

  // Demonstrates the use of quoted pattern matching
  // over a refined type extracting the tuple type
  // for e.g., MirroredElemLabels
  inline def test1[T](value: =>T): List[String] =
    ${ test1Impl('value) }

  def test1Impl[T](using s: Scope)(value: s.Expr[T])(using s.Type[T]): s.Expr[List[String]] = {
    import s.tasty._
    Expr.summon[Mirror.Of[T]].get match {
      case '{ $m: Mirror.ProductOf[T]{ type MirroredElemLabels = $t } } => {
        Expr(mirrorFields(t))
      }
    }
  }
}