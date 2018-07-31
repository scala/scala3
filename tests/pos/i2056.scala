object Test {
  transparent def crash() = {
    try {
      println("hi")
    } catch {
      case e: Exception =>
    }
  }

  def main(args: Array[String]): Unit = {
    crash()
  }
}
