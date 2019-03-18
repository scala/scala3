// This test case used to fail when mixin forwarders were generated before erasure,
// it doesn't anymore since the forwarders generated after erasure do not clash,
// the comments are preserved for posterity.

class Foo

// Using self-types to force mixin forwarders

trait OneA[X] {
  def concat(suffix: Int): X = ???
}

trait OneB[X] { self: OneA[X] =>
  override def concat(suffix: Int): X = ???
}

trait TwoA[Y <: Foo] {
  def concat[Dummy](suffix: Int): Y = ???
}

trait TwoB[Y <: Foo] { self: TwoA[Y] =>
  override def concat[Dummy](suffix: Int): Y = ???
}

class Bar1 extends OneA[Foo] with OneB[Foo]
  // Because mixin forwarders are generated before erasure, we get:
  //  override def concat(suffix: Int): Foo

class Bar2 extends Bar1 with TwoA[Foo] with TwoB[Foo] // error
  // We get a mixin forwarder for TwoB:
  //   override def concat[Dummy](suffix: Int): Foo
  // which gets erased to:
  //   override def concat(suffix: Int): Foo
  // This clashes with the forwarder generated in Bar1, and the compiler detects that:
  //
  // |class Bar2 extends Bar1 with TwoA[Foo] with TwoB[Foo]
  // |      ^
  // |      Name clash between defined and inherited member:
  // |      override def concat(suffix: Int): Foo in class Bar1 and
  // |      override def concat: [Dummy](suffix: Int): Foo in class Bar2
  // |      have the same type after erasure.
  //
  // But note that the compiler is able to see the mixin forwarder in Bar1
  // only because of joint compilation, this doesn't work with separate
  // compilation as in mixin-forwarder-clash2.
