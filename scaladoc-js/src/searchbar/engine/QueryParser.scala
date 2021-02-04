package dotty.tools.scaladoc

import scala.util.matching.Regex._
import scala.util.matching._

class QueryParser:
  val kinds = Seq(
    "class",
    "trait",
    "enum",
    "object",
    "def",
    "val",
    "var",
    "package",
    "given",
    "type"
  )
  val kindRegex = ("(?i)" + kinds.mkString("(","|",")") + " (.*)").r
  val restRegex = raw"(.*)".r
  val escapedRegex = raw"`(.*)`".r

  def parse(query: String): List[Matchers] = query match {
    case escapedRegex(rest) => List(Matchers.ByName(rest))
    case kindRegex(kind, rest) => List(Matchers.ByKind(kind)) ++ parse(rest)
    case restRegex(name) => List(Matchers.ByName(name))
    case _ => List()
  }