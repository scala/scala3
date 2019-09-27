object Test {

  def (x: Int) gt (y: Int) = x > y
  val y3 =
    if (1) max 10 gt 0  // error: end of statement expected but integer literal found // error // error // error
      1
    else  // error: end of statement expected but 'else' found
      2   // error
}
