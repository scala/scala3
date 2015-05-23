import scala.util.matching.Regex

object Test extends dotty.runtime.LegacyApp {
  val input = "CURRENCY 5.80"
  println("CURRENCY".r.replaceAllIn(input, Regex quoteReplacement "US$"))
}

