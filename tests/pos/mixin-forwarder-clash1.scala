// This test case used to fail when mixin forwarders were generated before erasure,
// it doesn't anymore since the forwarders generated after erasure do not clash,
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

class Bar2 extends Bar1 with Two[Foo] // error
  // We get a mixin forwarder for Two:
  //   override def concat[Dummy](suffix: Int): Foo
  // which gets erased to:
  //   override def concat(suffix: Int): Foo
  // This clashes with the forwarder generated in Bar1, and the compiler detects that:
  //
  // |class Bar2 extends Bar1 with Two[Foo]
  // |      ^
  // |      Name clash between defined and inherited member:
  // |      override def concat(suffix: Int): Foo in class Bar1 and
  // |      override def concat: [Dummy](suffix: Int): Foo in class Bar2
  // |      have the same type after erasure.
  //
  // But note that the compiler is able to see the mixin forwarder in Bar1
  // only because of joint compilation, this doesn't work with separate
  // compilation as in mixin-forwarder-clash2.
