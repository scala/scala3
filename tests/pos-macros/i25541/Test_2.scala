final case class MessageError[Node](message: Option[String], code: Option[Int], data: Option[Node])

object Test:
  def readWriter(custom: Custom): Unit =
    custom.macroRW[MessageError[Int]]
