class Bar2 extends Bar1 with Two[Foo] // error
  // We get a mixin forwarder for Two:
  //   override def concat(suffix: Int): Object
  // This clashes with the forwarder generated in Bar1, and
  // we can detect that even with separate compilation,
  // because forwarders are always generated after erasure
  // so their signature matches the erased signature of the
  // method they forward to, which double-defs check will
  // consider:
  //
  // |class Bar2 extends Bar1 with TwoA[Foo] with TwoB[Foo]
  // |      ^
  // |      Name clash between inherited members:
  // |      def concat(suffix: Int): X in trait One and
  // |      def concat: [Dummy](suffix: Int): Y in trait Two
  // |      have the same type after erasure.
