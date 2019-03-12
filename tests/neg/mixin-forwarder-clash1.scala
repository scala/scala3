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

class Bar2 extends Bar1 with Two[Foo] // error
  // We get a mixin forwarder for Two:
  //   override def concat(suffix: Int): Object
  // This clashes with the forwarder generated in Bar1, and the compiler detects that:
  //
  // |class Bar2 extends Bar1 with Two[Foo]
  // |      ^
  // |      Name clash between inherited members:
  // |      def concat(suffix: Int): X in trait One at line 4 and
  // |      def concat: [Dummy](suffix: Int): Y in trait Two at line 8
  // |      have the same type after erasure.
  //
  // This also works with separate compilation as demonstrated by
  // mixin-forwarder-clash2.
