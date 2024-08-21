
class Ann(x: Any) extends annotation.Annotation
object Message:
  implicit def toNoExplanation(str: String): Message @Ann(str) = ???
class Message

object report:
  def error(x: Message): Unit = ???

def test =
  report.error("a") // works
  report.error("a".stripMargin) // was an error
