import scala.caps.*

class Console() extends SharedCapability:
  def println(s: String): Unit = System.out.println(s"console: $s")

def log(s: String)(using Console): Unit =
  summon[Console].println(s)

object test:
  val addPure: (Int, Int) -> Int = (a, b) => a + b // same as before

  given Console = Console() // provide capability locally

  // and this is now considered pure somehow
  val addWritesToConsole: (Int, Int) -> Int = (a, b) => // error
    log(s"adding a ($a) to b ($b)")
    a + b