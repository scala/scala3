class Bar2 extends Bar1 with TwoA[Foo] with TwoB[Foo] // error
  // We get a mixin forwarder for TwoB:
  //   override def concat[Dummy](suffix: Int): Foo
  // which gets erased to:
  //   override def concat(suffix: Int): Foo
  // This clashes with the forwarder generated in Bar1, but
  // unlike with mixin-forwarder-clash1, the compiler
  // doesn't detect that due to separate compilation,
  // so this test case fails.
