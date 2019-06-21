import scala.compiletime._

object Test {

  def main(args: Array[String]): Unit = {
    fail(println("foo")) // error
  }

  inline def fail(p1: => Any) = error(code"failed: $p1 ...")

}
