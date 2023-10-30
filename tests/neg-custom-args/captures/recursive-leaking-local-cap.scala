import language.experimental.captureChecking
trait Cap:
  def use: Int = 42

def usingCap[sealed T](op: Cap^ => T): T = ???

def badTest(): Unit =
  def bad(b: Boolean)(c: Cap^): Cap^{cap[bad]} =  // error
    if b then c
    else
      val leaked = usingCap[Cap^{cap[bad]}](bad(true))
      leaked.use  // boom
      c

  usingCap[Unit]: c0 =>
    bad(false)(c0)

class Bad:
  def foo: Cap^{cap[Bad]} = ??? // error
  private def bar: Cap^{cap[Bad]} = ??? // ok


