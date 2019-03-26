  class BitMap
  class InkJet

  class Printer {
    type PrinterType
    def print(bits: BitMap): Unit = ???
    def status: List[String] = ???
    implied bitmap for BitMap
  }

  class Scanner {
    def scan(): BitMap = ???
    private def scanAll: BitMap = ???
    def status: List[String] = ???
  }

  class Copier {
    private val printUnit = new Printer { type PrinterType = InkJet }
    private val scanUnit = new Scanner

    export scanUnit.scanIt          // error: no eligible member
    export scanUnit.{scanAll => foo} // error: no eligible member
    export printUnit.{stat => _, _} // error: double definition
    export scanUnit._               // error: double definition
    export printUnit.bitmap         // error: no eligible member
    export implied printUnit.status // error: no eligible member

    def status: List[String] = printUnit.status ++ scanUnit.status
  }

trait IterableOps[+A, +CC[_], +C] {

  def concat[B >: A](other: List[B]): CC[B]

  export this.{concat => ++}   // error: no eligible member

}