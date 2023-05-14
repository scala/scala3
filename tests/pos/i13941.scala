import scala.annotation.tailrec

object A {
  def b = Option("a").map { x =>
    @tailrec
    def loop(): Int = {
      try
        2
      catch
        case _: Throwable =>
          loop()
    }
    x
  }
}
