//> using options -feature
package test
import language.experimental.into

object Test:
  given Conversion[Int, C] = C(_)

  def f(x: T) = ()
  f(1)      // ok

  given stringToKeyword: Conversion[String, Keyword] = Keyword(_)

  val dclKeywords = Set[Keyword]("def", "val") // ok
  val keywords = dclKeywords + "if" + "then" + "else" // ok


