object Test {
  def main(args: Array[String]): Unit = {
    ("": Any) match {
      case a @ Test => 1
      case _ => 2
    }
  }
}
