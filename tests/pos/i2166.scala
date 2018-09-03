object Test {
  rewrite def f = rewrite "" match { case _ => false }

  def main(args: Array[String]): Unit = f
}