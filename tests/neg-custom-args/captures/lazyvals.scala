import language.experimental.captureChecking
import caps.*

class Console extends SharedCapability:
  def println(msg: String): Unit = Predef.println("CONSOLE: " + msg)

@main def run =
  val console: Console^ = Console()
  lazy val x: () -> String = {
    console.println("Computing x")
    () => "Hello, World!"
  }

  val fun: () ->{console} String = () => x() // ok
  val fun2: () -> String = () => x() // error
  val fun3: () ->{x} String = () => x() // error // error

  println("Before accessing x")
  println(s"x = ${x()}")
  println(s"x again = ${x()}")
