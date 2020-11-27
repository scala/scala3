object Test {
  def main(args: Array[String]): Unit = {
    ("": Any) match {
      case Test as a => 1
      case _ => 2
    }
  }
}
