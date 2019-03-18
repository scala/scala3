class Bar2 extends Bar1 with TwoA[Foo] with TwoB[Foo] // error
  // We get a mixin forwarder for TwoB:
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
  // |      override def concat(suffix: Int): X in trait OneB and
  // |      override def concat: [Dummy](suffix: Int): Y in trait TwoB
  // |      have the same type after erasure.
