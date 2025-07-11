import caps.{Capability, Control}

class Try[+T]
case class Ok[T](x: T) extends Try[T]
case class Fail(ex: Exception) extends Try[Nothing]

object Try:
  def apply[T](body: => T): Try[T]^{body.only[Control]} =
    try Ok(body)
    catch case ex: Exception => Fail(ex)
