object Test {
  inline def f = inline "" match { case _ => false }

  def main(args: Array[String]): Unit = f
}