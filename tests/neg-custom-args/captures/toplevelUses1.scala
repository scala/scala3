package test
import caps.*

class Stream extends SharedCapability:
  def println(): Unit = ()

object Console:
  val out: Stream = new Stream()
  def println(): Unit = out.println()

object M:
  val c: Stream = new Stream()

object Console2 uses Console:
  def println() = Console.println()

object N:
  def test =
    val f = () =>
      M.c.println()      // error
      Console.println()  // error
      Console2.println() // error
  val g =
    M.c.println()        // error
    Console.println()    // error
    Console2.println()   // error
    22
  M.c.println()          // error
  Console.println()      // error

def test2 =
  M.c.println()          // error
  Console.println()      // error
