class BitMap
class InkJet

class Printer {
  type PrinterType
  def print(bits: BitMap): Unit = ???
  def status: List[String] = ???
}

class Scanner {
  def scan(): BitMap = ???
  def status: List[String] = ???
}

class Copier {
  private val printUnit = new Printer { type PrinterType = InkJet }
  private val scanUnit = new Scanner

  export scanUnit.scan
  export printUnit.{status => _, _}

  def status: List[String] = printUnit.status ++ scanUnit.status
}

class C22 { type T }
object O22 { val c: C22 = ??? }
export O22.c
def f22: c.T = ???

object O:
  class C23(val x: Int)
  def m23(c: C23): Int = c.x + 1
export O.*