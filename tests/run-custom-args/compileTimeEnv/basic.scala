import scala.compiletime.*

object Test {

  inline def logEnv(inline k: String): Unit =
    inline envGet(k) match
      case Some(v) => println(s"$k = [$v]")
      case None    => println(k + " is not defined")

  def main(args: Array[String]): Unit = {
    logEnv("a")
    logEnv("b")
    logEnv("c.b.a")
    logEnv("wat")
  }
}
