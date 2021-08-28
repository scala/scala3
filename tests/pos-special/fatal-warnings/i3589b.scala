class Test {
  def test(x: 1 | 2 | 3) = (x: @annotation.switch) match {
    case 1 => 1
    case 2 => 2
    case 3 => 3
  }
}
