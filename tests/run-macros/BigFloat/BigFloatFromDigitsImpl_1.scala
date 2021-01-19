package test
import language.experimental.genericNumberLiterals
import scala.util.FromDigits
import scala.quoted._

object BigFloatFromDigitsImpl with
  def apply(digits: Expr[String])(using Quotes): Expr[BigFloat] =
    digits.value match
      case Some(ds) =>
        try
          val BigFloat(m, e) = BigFloat(ds)
          '{BigFloat(${Expr(m)}, ${Expr(e)})}
        catch case ex: FromDigits.FromDigitsException =>
          import quotes.reflect.report
          report.error(ex.getMessage)
          '{BigFloat(0, 0)}
      case None =>
        '{BigFloat($digits)}
