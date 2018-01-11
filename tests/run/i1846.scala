object Test {
  def main(args: Array[String]): Unit = {
    val x = 42

    x match {
      case {42}.toString => println(42)
      case _ => println(0)
    }

    42 match { case { 42 } => println(42) }

    42 match {
      case { 42 }.toString => println(42)
      case _ => println(0)
    }

    "h" match {
      case { 42.toString } => println(42)
      case _ => println(0)
    }
  }
}
