import language.experimental.genericNumberLiterals
object Test extends App {

  val e1: Even = 1234
  val e2: Even = 123  // error: 123 is odd
  val e3: Even = 123456789101111 // error: number too large
}