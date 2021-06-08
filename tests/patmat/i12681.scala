object Examples {

  case class Leaf1() extends i.Root
  case class Leaf2() extends i.Branch

  val i = new Inner()

  class Inner {

    sealed trait Root
    sealed trait Branch extends Root

    // simulate ordinal method of a Mirror.SumOf generated at this call site
    def myOrdinal(r: Root): Int = r match {
      case _: Examples.Leaf1  => 0
      case _: Inner.this.Branch => 1
    }
  }

}