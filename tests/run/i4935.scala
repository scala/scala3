object Test {
  val (Some(a), b) = (Some(3), 6)

  def main(args: Array[String]) = assert(a == 3)
}
