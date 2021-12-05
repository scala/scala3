object Test {
  def main(args: Array[String]): Unit = {
    assert(math.round(123456789) == 123456789) // error
    assert(math.round(1234567890123456789L) == 1234567890123456789L) // error
  }
}
