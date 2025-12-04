//> using options -Yimports:scala,scala.Predef
import language.experimental.captureChecking
import caps.*

class Console extends SharedCapability:
  def println(msg: String): Unit = Predef.println("CONSOLE: " + msg)

class IO extends SharedCapability:
  def readLine(): String = scala.io.StdIn.readLine()

@main def run =
  val console: Console^ = Console()
  val io: IO^ = IO()
  lazy val x: () ->{io} String = {
    console.println("Computing x")
    () => io.readLine()
  }

  val fun: () ->{console,io} String = () => x() // ok
  val fun2: () ->{io} String = () => x() // error
