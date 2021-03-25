import test.BigFloat
object Test extends App {
  val x: BigFloat = 1234.45e3333333333  // error: exponent too large
}