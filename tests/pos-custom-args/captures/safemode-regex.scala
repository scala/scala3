import language.experimental.safe
import scala.util.matching.Regex

object Test:
  val date = new Regex("""(\d{4})-(\d{2})-(\d{2})""")
  val word = "\\w+".r
  val anyDate = date.unanchored

  def all(s: String): List[String] = word.findAllIn(s).toList
  def first(s: String): Option[String] = word.findFirstIn(s)
  def firstMatch(s: String): Option[Regex.Match] = date.findFirstMatchIn(s)
  def prefix(s: String): Option[String] = word.findPrefixOf(s)
  def matchesAll(s: String): Boolean = date.matches(s)
  def split(s: String): Array[String] = word.split(s)
  def allMatches(s: String): List[Regex.Match] = date.findAllMatchIn(s).toList
  def matchData(s: String): List[Regex.Match] = word.findAllIn(s).matchData.toList

  def year(s: String): String = s match
    case date(y, _, _) => y
    case anyDate(y, _, _) => y
    case _ => ""

  def extractors(s: String): String = date.findFirstMatchIn(s) match
    case Some(Regex.Match(whole)) => whole
    case _ => ""

  def groups(s: String): String = date.findFirstMatchIn(s) match
    case Some(Regex.Groups(y, m, d)) => s"$d/$m/$y"
    case _ => ""

  def upper(s: String): String = word.replaceAllIn(s, m => m.matched.toUpperCase)
  def some(s: String): String = word.replaceSomeIn(s, m => if m.matched.length > 3 then Some("*") else None)
  def replaceFirst(s: String): String = word.replaceFirstIn(s, Regex.quoteReplacement("$1"))
  def literal(s: String): Regex = Regex.quote(s).r

  def replaceAllInWithLog(s: String, log: String => Unit): String =
    word.replaceAllIn(s, m => { log(m.matched); m.matched.reverse })

  def iterate(s: String): Int =
    val it = word.findAllIn(s)
    var n = 0
    while it.hasNext do
      it.next()
      n += it.start + it.end
    n
