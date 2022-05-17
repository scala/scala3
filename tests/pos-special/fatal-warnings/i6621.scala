object Unapply {
  def unapply(a: Any): Option[(Int, Int)] =
    Some((1, 2))
}

object Test {
  val Unapply(x, y) = "": @unchecked
}
