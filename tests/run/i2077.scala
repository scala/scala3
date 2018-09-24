object Test {
  inline val x = true
  val y = if (x) 1 else 2  // reduced to val y = 1

  def main(args: Array[String]): Unit  =
    if ({ println("hi"); true })  // cannot be reduced
      1
    else
      2
}
