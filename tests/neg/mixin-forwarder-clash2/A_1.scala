class Foo

// Using self-types to force mixin forwarders

trait OneA[X] {
  def concat(suffix: Int): X = ???
}

trait OneB[X] { self: OneA[X] =>
  override def concat(suffix: Int): X = ???
}

trait TwoA[Y/* <: Foo*/] {
  def concat[Dummy](suffix: Int): Y = ???
}

trait TwoB[Y/* <: Foo*/] { self: TwoA[Y] =>
  override def concat[Dummy](suffix: Int): Y = ???
}

class Bar1 extends OneA[Foo] with OneB[Foo]
// Because mixin forwarders are generated after erasure, we get:
//  override def concat(suffix: Int): Object
