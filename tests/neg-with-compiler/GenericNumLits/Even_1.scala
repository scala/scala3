import scala.util.FromDigits
import scala.quoted._
import scala.quoted.matching._

case class Even(n: Int)
object Even {

  private def evenFromDigits(digits: String): Even = {
    val intValue = FromDigits.intFromDigits(digits)
    if (intValue % 2 == 0) Even(intValue)
    else throw FromDigits.MalformedNumber(s"$digits is odd")
  }

  private def evenFromDigitsImpl(digits: Expr[String])(given ctx: QuoteContext): Expr[Even] = digits match {
    case Const(ds) =>
      val ev =
        try evenFromDigits(ds)
        catch {
          case ex: FromDigits.FromDigitsException =>
            ctx.error(ex.getMessage)
            Even(0)
        }
      '{Even(${Expr(ev.n)})}
    case _ =>
      '{evenFromDigits($digits)}
  }

  class EvenFromDigits extends FromDigits[Even] {
    def fromDigits(digits: String) = evenFromDigits(digits)
  }

  given EvenFromDigits {
    override inline def fromDigits(digits: String) = ${
      evenFromDigitsImpl('digits)
    }
  }
}
