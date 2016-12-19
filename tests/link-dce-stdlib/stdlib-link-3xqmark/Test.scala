import scala.annotation.internal

object Test {
  def main(args: Array[String]): Unit = {
    try {
      ???
    } catch {
      case _: NotImplementedError => System.out.println(42)
    }
  }
}
