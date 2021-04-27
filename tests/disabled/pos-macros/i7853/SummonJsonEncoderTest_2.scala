import scala.deriving._
import scala.quoted._
import JsonEncoder.{given, _}

object SummonJsonEncoderTest {

  inline def encodeAndMessAroundType[T](value: =>T): String = ${ encodeAndMessAroundTypeImpl('value) }

  def encodeAndMessAroundTypeImpl[T: Type](value: Expr[T])(using Quotes): Expr[String] = {
    import quotes.reflect._

    val mirrorExpr = Expr.summon[Mirror.Of[T]] match {
      case Some(mirror) => mirror
    }

    '{
      given JsonEncoder[T] = JsonEncoder.derived($mirrorExpr)
      val encoder = summon[JsonEncoder[T]]
      encoder.encode($value)
    }
  }
}