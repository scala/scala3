// This test case was supposed to fail when mixin forwarders were generated before erasure,
// but didn't due to separate compilation unlike mixin-forwarder-clash1,
// it's not supposed to fail anymore since the forwarders generated after erasure do not clash,
// the comments are preserved for posterity.

class Foo

trait One[X] {
  def concat(suffix: Int): X = ???
}

trait Two[Y <: Foo] {
  def concat[Dummy](suffix: Int): Y = ???
}

class Bar1 extends One[Foo]
// Because mixin forwarders are generated before erasure, we get:
//  override def concat(suffix: Int): Foo
