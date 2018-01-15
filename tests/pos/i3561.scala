class Test {
  val Constant = 'Q' // OK if final
  def tokenMe(ch: Char) = (ch: @annotation.switch) match {
    case ' ' => 1
    case 'A' => 2
    case '5' | Constant => 3
  }

  def test2(x: Any) = x match {
    case 1 => 1
    case 2 => 2
    case 3 => 3
    case x: String => 4
  }
}
