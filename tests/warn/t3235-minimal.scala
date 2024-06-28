//> using options  -deprecation

object Test {
  def main(args: Array[String]): Unit = {
    assert(123456789.round == 123456789) // warn
    assert(math.round(123456789) == 123456789) // warn
    assert(1234567890123456789L.round == 1234567890123456789L) // warn
    assert(math.round(1234567890123456789L) == 1234567890123456789L) // warn
  }
}