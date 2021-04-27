import language.experimental.genericNumberLiterals
object Test extends App {

  val e1: Even = 1234
  val e2: Even = 123  // error: 123 is odd
}