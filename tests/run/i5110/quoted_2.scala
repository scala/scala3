import scala.util.Try

object Test {
  import Macro._

  def main(args: Array[String]): Unit = {
    def bomb = new Bomb
    new Bomb().bar() // Safely elided prefix
    bomb.bar() // Safely elided prefix
    shouldThrowBoom { new Bomb().foo() }
    shouldThrowBoom { bomb.foo() }

  }

  def shouldThrowBoom(x: => Any): Unit = {
    try {
      x
      ???
    } catch {
      case ex: Boom => // OK
    }
  }
}
