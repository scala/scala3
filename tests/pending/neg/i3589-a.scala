object Test {
  case class IntAnyVal(x: Int) extends AnyVal

  def test(x: IntAnyVal) = (x: @annotation.switch) match { //error: warning: could not emit switch
    case IntAnyVal(1) => 1
    case IntAnyVal(2) => 2
    case IntAnyVal(3) => 3
    case _ => 4
  }
}
