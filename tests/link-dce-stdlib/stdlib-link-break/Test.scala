
object Test {
  def main(args: Array[String]): Unit = {
    import scala.util.control.Breaks._
    breakable {
      while (true) {
        System.out.println(42)
        break()
      }
    }
  }
}
