
object Example {
  val badsingle = "foo \unope that's wrong" // error
  val caretPos = "foo \u12x3 pos @ x" // error
  val caretPos2 = "foo \uuuuuuu12x3 pos @ x" // error
  val carPosTerm = "foo \u123" // error
  val halfAnEscape = "foo \u12" // error
  val halfAnEscapeChar = '\u45' // error
  val `half An Identifier\u45` = "nope" // error
}