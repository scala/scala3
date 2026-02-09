import scala.util.matching.Regex

object Test extends App {
  def head(s: String, r: Regex): Option[(String | Null, String | Null)] =
    s.trim match {
      case r(hd, tl) => Some((hd, tl))  // error // error // error
      case _ => None
    }
}