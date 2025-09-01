import scala.util.FromDigits

object Test extends App {

  val x: BigInt = 13232202002020202020202 // error
  val z: BigDecimal = 132322020020.223

  case class Even(n: Int)

  given FromDigits[Even]:
    def fromDigits(digits: String): Even =
      val intValue = digits.toInt
      if (intValue % 2 == 0) Even(intValue)
      else throw FromDigits.MalformedNumber()

  val e: Even = 1234 // error

  try {
    println(123: Even) // error
  } catch {
    case ex: FromDigits.MalformedNumber => println("malformed")
  }

  x match {
    case 13_232_202_002_020_202_020_202 => () // error
  }
  (x: Any) match {
    case 13232202002020202020202: BigInt => () // error
  }
  x match {
    case 13232202002020202020202 => assert(false) // error
    case -0xaabb12345ACF12345AC => () // error
  }

  (e: Any) match {
    case 1234: Even => // error
    case _: Even =>
  }
}