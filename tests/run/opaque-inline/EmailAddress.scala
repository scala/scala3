import scala.quoted.*

opaque type EmailAddress = String
object EmailAddress extends EmailAddressOps[EmailAddress]:

  given (using s: ToExpr[String]): ToExpr[EmailAddress] = s

  def parse(s: String): Either[String, EmailAddress] =
    if (s contains "@") Right(s)
    else Left("No @ symbol")
