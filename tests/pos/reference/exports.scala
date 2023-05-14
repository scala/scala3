object exports:
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

  class C { type T }
  object O { val c: C = ??? }
  export O.c
  def f: c.T = ???

  class StringOps(x: String):
    def *(n: Int): String = ???
    def ::(c: Char) = c.toString + x
    def capitalize: String = ???

  extension (x: String)
    def take(n: Int): String = x.substring(0, n)
    def drop(n: Int): String = x.substring(n)
    private def moreOps = new StringOps(x)
    export moreOps.*

  val s = "abc"
  val t1 = (s.take(1) + s.drop(1)).capitalize * 2
  val t2 = 'a' :: s
