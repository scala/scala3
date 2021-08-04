enum Color(val x: Int) {
  case Green  extends Color(3)
  // case Red    extends Color(2)
  case Violet extends Color(Green.x + 1)
  // case RGB(xx: Int) extends Color(xx)
}
