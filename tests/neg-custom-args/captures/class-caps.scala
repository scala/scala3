//> using scala 3.nightly

import scala.language.experimental.captureChecking
import scala.caps.*

class Console() extends SharedCapability:
  def println(s: String): Unit = System.out.println(s"console: $s")

def log(s: String)(using c: Console): Unit =
  summon[Console].println(s)

class Test:
  this: Test^ =>
  val addPure: (Int, Int) -> Int = (a, b) => a + b // same as before

  val console: Console^ = Console() // provide capability locally

  def addWritesToConsole: (Int, Int) -> Int = (a, b) => // error
    log(s"adding a ($a) to b ($b)")(using console)
    a + b


class Test1:
  val addPure: (Int, Int) -> Int = (a, b) => a + b // same as before

  val console: Console^ = Console() // provide capability locally

  def addWritesToConsole: (Int, Int) -> Int = (a, b) => // error
    log(s"adding a ($a) to b ($b)")(using console)
    a + b

object Test2:
  val addPure: (Int, Int) -> Int = (a, b) => a + b // same as before

  val console: Console^ = Console() // provide capability locally

  def addWritesToConsole: (Int, Int) -> Int = (a, b) => // error
    log(s"adding a ($a) to b ($b)")(using console)
    a + b
