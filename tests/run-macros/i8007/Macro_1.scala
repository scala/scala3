import scala.deriving._
import scala.quoted._
import scala.quoted.matching._
import scala.compiletime.{erasedValue, summonFrom, constValue}

object Macro {
  case class Person(name: String, age: Int)

  def mirrorFields[T](t: Type[T])(given qctx: QuoteContext): List[String] =
    t match {
      case '[$field *: $fields] => field.show :: mirrorFields(fields)
      case '[Unit] => Nil
    }

  inline def usingSummonFrom[T](value: =>T): List[String] =
    ${ usingSummonFromImpl('value) }

  def usingSummonFromImpl[T: Type](value: Expr[T])(given qctx: QuoteContext): Expr[List[String]] = {
    import qctx.tasty.{_, given}


    val mirrorTpe = '[Mirror.Of[T]]

    summonExpr(given mirrorTpe).get match {
      case '{ $m: Mirror.ProductOf[T] } => {
        val typeMember = TypeSelect(m.unseal, "MirroredElemLabels")

        type TT
        implicit val TT: quoted.Type[TT] = typeMember.tpe.seal.asInstanceOf[quoted.Type[TT]]

        Expr(mirrorFields('[TT]))
      }
    }
  }
}