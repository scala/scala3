object MatchTypes:

  type ConstituentPartOf[A] = A match
    case BigInt => Int
    case String => Char

  def lastPartOf[A](thing: A): ConstituentPartOf[A] = thing match
    case number: BigInt => (number % 10).toInt
    case string: String => string.charAt(string.length - 1)

  def main(args: Array[String]): Unit =
    val lastPartOfSomethingElse = lastPartOf(BigDecimal("10")) // warn
    //                                                ^
    // Match type reduction failed since selector BigDecimal
    // matches none of the cases
    println(lastPartOfSomethingElse)
