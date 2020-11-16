package test
import language.experimental.genericNumberLiterals
import scala.util.FromDigits
import scala.quoted._

object BigFloatFromDigitsImpl:
  def apply(digits: Expr[String])(using QuoteContext): Expr[BigFloat] =
    digits match
      case Const(ds) =>
        try
          val BigFloat(m, e) = BigFloat(ds)
          '{BigFloat(${Expr(m)}, ${Expr(e)})}
        catch case ex: FromDigits.FromDigitsException =>
          report.error(ex.getMessage)
          '{BigFloat(0, 0)}
      case digits =>
        '{BigFloat($digits)}
