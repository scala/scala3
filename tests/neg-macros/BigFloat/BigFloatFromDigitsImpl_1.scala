package test
import scala.util.FromDigits
import scala.quoted._

object BigFloatFromDigitsImpl:
  def apply(using s: Scope)(digits: s.Expr[String]): s.Expr[BigFloat] =
    digits match
      case Const(ds) =>
        try
          val BigFloat(m, e) = BigFloat(ds)
          '{BigFloat(${s.Expr(m)}, ${s.Expr(e)})}
        catch case ex: FromDigits.FromDigitsException =>
          report.error(ex.getMessage)
          '{BigFloat(0, 0)}
      case digits =>
        '{BigFloat($digits)}
