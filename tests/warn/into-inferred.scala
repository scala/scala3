//> using options  -feature

import language.experimental.into
import Conversion.{into, underlying}

trait Token
class Keyword(str: String)
case class Phrase(words: into[Keyword]*)

object Test:
  given Conversion[String, Keyword] = Keyword(_)

  val xs = List[into[Keyword]]("if", "then", "else") // ok
  val _: List[Keyword] = xs.map(_.underlying)

  val p = Phrase("if", "then", "else") // ok
  val ws = p.words
  val _: Seq[Keyword] = ws

  val p2 = Phrase(xs*) // ok

  val ifKW: into[Keyword] = "if"
  val ys: List[into[Keyword]] = List(ifKW, "then", "else")         // warn // warn
