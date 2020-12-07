package test
import language.experimental.genericNumberLiterals
import scala.util.FromDigits
import scala.quoted._

object BigFloatFromDigitsImpl:
  def apply(digits: Expr[String])(using Quotes): Expr[BigFloat] =
    digits.value match
      case Some(ds) =>
        try
          val BigFloat(m, e) = BigFloat(ds)
          '{BigFloat(${Value(m)}, ${Value(e)})}
        catch case ex: FromDigits.FromDigitsException =>
          quotes.reflect.report.error(ex.getMessage)
          '{BigFloat(0, 0)}
      case None =>
        '{BigFloat($digits)}
