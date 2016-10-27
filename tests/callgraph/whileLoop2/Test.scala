
object Test {
  def main(args: Array[String]): Unit = {
    var b = true
    do {
      b = false
    } while (b)
    System.out.println(42)
  }
}
