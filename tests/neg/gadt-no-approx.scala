object `gadt-no-approx` {
  def fo[U](u: U): U =
    (0 : Int) match {
      case _: u.type =>
        val i: Int = (??? : U) // error
        // potentially could compile
        // val i2: Int = u
        u
    }
}
