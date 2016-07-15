object Test {
  def x(in: (Int, Boolean)) = in match {
    case (i: Int, b: Boolean) => 3
  }
}