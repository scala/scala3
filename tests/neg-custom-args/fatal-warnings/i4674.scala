class Test {
  def test(x: String) = {
    x.foreach {
      case 's' => println("s")
      case c: Char => println(c) // error: type test always succeeds
    }
  }
}
