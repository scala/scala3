//> using options -Werror
import scala.language.experimental.subCases

enum Color:
  case Red, Green, Blue

def guardMakesPartial(c: Color, flag: Boolean): String = c match
  case c1 if flag if c1 match
    case Color.Red => "red"
    case Color.Green => "green"
    case Color.Blue => "blue"
  case _ => "fallback"

def literalFallback(n: Int): String = n match
  case x if x match
    case 1 => "one"
    case 2 => "two"
  case _ => "other"

enum Version:
  case Legacy
  case Stable

case class Document(version: Version)

def partialFieldAccess(d: Document): String = d match
  case x if x.version match
    case Version.Legacy => "legacy"
  case _ => "other"
