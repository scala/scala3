object Main {
  def main(args: Array[String]): Unit = {
    (1: Any) match {
      case x: String | Int => "OK"
      case (_: String) | (_: Int) => "OK"
      case (s: String) | _: Int => s   // error: Illegal variable in pattern alternative
    }
  }
}
