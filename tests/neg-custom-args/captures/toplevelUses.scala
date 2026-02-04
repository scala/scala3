package test
import caps.*

class Console extends SharedCapability:
  def println(): Unit = ()

object M:
  val c: Console^ = Console()

def test(cc: Console^) =
  val f = () => M.c.println()
  val _: () ->{M.c} Unit = f
  val _: () -> Unit = f // error
  val g = () => cc.println()
  val _: () ->{cc} Unit = g
  val _: () -> Unit = g // error

  Console.println()
  M.c.println()
