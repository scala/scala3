import test.BigFloat
import language.experimental.genericNumberLiterals
import scala.util.FromDigits
object Test extends App {


  println(BigFloat("1234.45"))
  println(BigFloat("1234.45" ++ "678"))
  println(BigFloat("1234.45e3"))
  println(BigFloat("-0.123E+1000"))
  try println(BigFloat("1234.45e3333333333"))
  catch {
    case ex: FromDigits.FromDigitsException => println("too large")
  }

  def check(x: BigFloat, digits: String) =
    assert(x == BigFloat(digits), x)

  check(1234.45, "1234.45")
  check(1234.45e3, "1234.45e3")
}