import scala.annotation.internal

object Test {
  def main(args: Array[String]): Unit = {

    // Loading Predef requires breaks
    import scala.util.control.Breaks._
    breakable {
      while (true) {
        System.out.println("break")
        break()
      }
    }


    try {
      ???
    } catch {
      case _: NotImplementedError => System.out.println("???")
    }

    println(42)
  }
}
