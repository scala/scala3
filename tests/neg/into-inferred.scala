//> using options  -feature -preview
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

  val s = Set(ifKW)
  val s1 = s + "then" + "else"
  val _: Set[into[Keyword]] = s1
  val s2 = s ++ List("then", "else")
  val s3: Set[into[Keyword] | String] = s2
  val s4 = s3.map(_.underlying)
  val _: Set[Keyword | String] = s4


  val l = List(ifKW)
  val l1: List[into[Keyword]] = l :+ "then" :+ "else"  // error
  val l2: List[into[Keyword]] = l ++ List("then", "else")  // warn // warn


