import scala.util.FromDigits
import scala.quoted._
import Even._

object EvenFromDigitsImpl:
  def apply(using s: Scope)(digits: s.Expr[String]): s.Expr[Even] = digits match {
    case Const(ds) =>
      val ev =
        try evenFromDigits(ds)
        catch {
          case ex: FromDigits.FromDigitsException =>
            report.error(ex.getMessage)
            Even(0)
        }
      '{Even(${Expr(ev.n)})}
    case _ =>
      '{evenFromDigits($digits)}
  }
