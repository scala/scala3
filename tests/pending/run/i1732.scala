object Test {
  import scala.util.control.Breaks
  def main(args: Array[String]): Unit = {
    Breaks.breakable {
      Breaks.break
    }
  }
}
