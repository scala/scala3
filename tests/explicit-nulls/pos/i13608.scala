import scala.util.control.NoStackTrace

case class ParseException(line: Int, character: Int, message: String) extends NoStackTrace
