object selftypes {

  trait A { self: AB =>

    type AA = List[this.BX]

    class AX

  }

  trait B { self: AB =>

    type BB = AA

    class BX
  }

  class AB extends A with B

}
