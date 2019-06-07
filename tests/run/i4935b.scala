object Test {
  val (a, _, b) = (Some(3), 6, Some(9))

  def main(args: Array[String]) = assert(a == Some(3) && b == Some(9))
}
