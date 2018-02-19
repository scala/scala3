class Test {
  val Constant = 'Q' // OK if final
  def tokenMe(ch: Char) = (ch: @annotation.switch) match { // error: could not emit switch
    case ' ' => 1
    case 'A' => 2
    case '5' | Constant => 3
    case '4' => 4
  }

  def test2(x: Any) = (x: @annotation.switch) match { // error: could not emit switch
    case ' ' => 1
    case 'A' => 2
    case '5' | Constant => 3
    case '4' => 4
  }

  def test3(x: Any) = (x: @annotation.switch) match { // error: could not emit switch - too few cases
    case 1 => 1
    case 2 => 2
    case x: String => 4
  }
}
