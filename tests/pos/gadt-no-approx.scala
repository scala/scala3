object `gadt-no-approx` {
  def fo[U](u: U): U =
    (0 : Int) match {
      case _: u.type =>
        val i: Int = (??? : U)
        val i2: Int = u
        u
    }
}
