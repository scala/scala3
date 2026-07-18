object Test {
  val Yes1 = true
  val Yes2 = true
  val No1 = false
  val No2 = false

  def test(b1: Boolean, b2: Boolean) = {
    (b1, b2) match {
      case (No1, No2) => println("1")
      case (No1, Yes2) => println("2")
      case (Yes1, No2) => println("3")
      case (Yes1, Yes2) => println("4")
    }
  }

  test(No1, Yes2)
}