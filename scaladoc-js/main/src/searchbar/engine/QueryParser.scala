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
  val nameRegex = raw"(.*)".r
  val escapedRegex = raw"`(.*)`".r
  val signatureRegex = raw"(.*=>.*)".r

  def parseMatchers(query: String): EngineQuery = query match {
    case escapedRegex(rest) => NameAndKindQuery(Some(rest), None)
    case kindRegex(kind, rest) => NameAndKindQuery(Some(rest), Some(kind))
    case nameRegex(name) => NameAndKindQuery(Some(name), None)
    case _ => NameAndKindQuery(None, None)
  }

  def parse(query: String): EngineQuery = query match {
    case signatureRegex(signature) => SignatureQuery(signature)
    case other => parseMatchers(other)
  }