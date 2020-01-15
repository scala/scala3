import scala.deriving._
import scala.quoted._
import scala.quoted.matching._
import scala.compiletime.{erasedValue, summonFrom, constValue}

object Macro {
  case class Person(name: String, age: Int)

   // Summon a mirror for a particular type
  inline def summonMirror[T]: Option[Mirror.Of[T]] =
    summonFrom {
      case given m: Mirror.Of[T] => Some(m)
      case _ => None
    }

  // Get fields from a mirror:
  inline def mirrorFields[Fields <: Tuple]: List[String] =
    inline erasedValue[Fields] match {
      case _: (field *: fields) => constValue[field].toString :: mirrorFields[fields]
      case _ => Nil
    }

  inline def usingSummonFrom[T](value: =>T): String =
    ${ usingSummonFromImpl('value, summonMirror[T]) }

  def usingSummonFromImpl[T: Type](value: Expr[T], m: Option[Mirror.Of[T]])(given qctx: QuoteContext): Expr[String] = {
    import qctx.tasty.{_, given}
    val theMirror = m match { case Some(mirror) => mirror }

    theMirror match {
      case m: Mirror.ProductOf[T] => println("it's a product: " + mirrorFields[m.MirroredElemLabels])
    }

    '{ "Doesn't matter" }
  }
}