class Foo

trait One[X] {
  def concat(suffix: Int): X = ???
}

trait Two[Y/* <: Foo*/] {
  def concat[Dummy](suffix: Int): Y = ???
}

class Bar1 extends One[Foo]
// Because mixin forwarders are generated after erasure, we get:
//  override def concat(suffix: Int): Object
