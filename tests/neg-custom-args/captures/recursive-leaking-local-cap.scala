import language.experimental.captureChecking
trait Cap:
  def use: Int = 42

def usingCap[sealed T](op: Cap^ => T): T = ???

def badTest(): Unit =
  def bad(b: Boolean)(c: Cap^): Cap^{c} =
    if b then c
    else
      val leaked = usingCap(bad(true)) // error
      leaked.use  // boom
      c

  usingCap[Unit]: c0 =>
    bad(false)(c0)



