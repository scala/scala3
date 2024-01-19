  class BitMap
  class InkJet

  class Printer {
    type PrinterType
    def print(bits: BitMap): Unit = ???
    def status: List[String] = ???
    given bitmap: BitMap with {}
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
    export printUnit.bitmap
    export printUnit.status

    def status: List[String] = printUnit.status ++ scanUnit.status // error: double definition w/ printUnit.status
  }

  trait IterableOps[+A, +CC[_], +C] {

    def concat[B >: A](other: List[B]): CC[B]

    export this.{concat => ++}   // error: no eligible member

  }

  class Foo {
    val foo : Foo = new Foo
    export foo.foo // error: no eligible member
  }

  class Baz {
    val bar: Bar = new Bar
    export bar._
  }
  class Bar {
    val baz: Baz = new Baz
    export baz._
  }

  object No:
    def printer =
      println("new Printer")
      new Printer
    export printer.*  // error: not stable
