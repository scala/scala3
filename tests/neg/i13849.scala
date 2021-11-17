import annotation.experimental
import language.experimental.saferExceptions

@experimental
case class Ex(i: Int) extends Exception(s"Exception: $i")

@experimental
def foo(): Unit throws Ex = throw Ex(1)

@experimental
object Main:
  def main(args: Array[String]): Unit =
    try
      foo()
    catch
      case _: Ex if false => println("Caught") // error
