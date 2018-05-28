import scala.util.control.NonFatal

object Test {
  try 2/0 catch {
    case NonFatal(ex) =>
  }
}