class Test {
  def test(x: String) = {
    x.foreach {
      case 's' => println("s")
      case c: Char => println(c) // should compile without warning
    }
  }
}
