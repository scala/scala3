import language.experimental.genericNumberLiterals
import scala.util.FromDigits
import scala.quoted.*


case class Even(n: Int)
object Even {

  def evenFromDigits(digits: String): Even = {
    val intValue = FromDigits.intFromDigits(digits)
    if (intValue % 2 == 0) Even(intValue)
    else throw FromDigits.MalformedNumber(s"$digits is odd")
  }

  class EvenFromDigits extends FromDigits[Even] {
    def fromDigits(digits: String) = evenFromDigits(digits)
  }

  given EvenFromDigits {
    override inline def fromDigits(digits: String) = ${
      EvenFromDigitsImpl('digits)
    }
  }
}
