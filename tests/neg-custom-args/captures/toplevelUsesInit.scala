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
  M.c.println()      // error
  Console.println()  // error
  Console2.println() // error

object N2 uses_init Console, Console2, M.c:
  M.c.println()
  Console.println()
  Console2.println()

val _ = M.c.println()          // error
val _ = Console.println()      // error
