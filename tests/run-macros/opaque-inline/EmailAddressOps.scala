import scala.quoted.*

trait EmailAddressOps[EmailAddressTransparent <: String]:

  inline def apply(inline s: String): EmailAddress =
    ${ EmailAddressOps.applyImpl('s) }

  private val pattern = java.util.regex.Pattern.compile("([^@]*)@([^@]*)")

  extension (value: EmailAddressTransparent)
    inline def localPart: String =
      val matcher = pattern.matcher(value: String)
      matcher.matches
      matcher.group(1)
    inline def domainPart: String =
      val matcher = pattern.matcher(value: String)
      matcher.matches
      matcher.group(2)

object EmailAddressOps {
  def applyImpl(expr: Expr[String])(using Quotes): Expr[EmailAddress] =
    import quotes.reflect.*
    expr.asTerm match
      case Inlined(_, _, Literal(StringConstant(s))) =>
        EmailAddress.parse(s) match
          case Right(email) => Expr(email)
          case Left(err) =>
            report.error(s"Not a valid email address: $err", expr)
            '{???}
      case _ =>
        report.error(s"Not a constant", expr)
        '{???}
}