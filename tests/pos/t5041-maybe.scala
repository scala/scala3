//> using options -Yexplicit-nulls
import language.experimental.magic
case class Token(text: String, startIndex: Int)

object Comment {
  def unapply(s: String): Token? = null
}

object HiddenTokens {
  "foo" match { case Comment(_) => }
}
