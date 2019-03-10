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

class Bar2 extends Bar1 with TwoA[Foo] with TwoB[Foo] // error
  // We get a mixin forwarder for TwoB:
  //   override def concat(suffix: Int): Object
  // This clashes with the forwarder generated in Bar1, and the compiler detects that:
  //
  // |class Bar2 extends Bar1 with TwoA[Foo] with TwoB[Foo]
  // |      ^
  // |   Name clash between inherited members:
  // |   override def concat(suffix: Int): Object in trait OneB at line 10 and
  // |   override def concat: [Dummy](suffix: Int): Y in trait TwoB at line 18
  // |   have the same type after erasure.
  //
  // This also works with separate compilation as demonstrated by
  // mixin-forwarder-clash2.
