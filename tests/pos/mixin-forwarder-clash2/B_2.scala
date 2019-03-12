// This test case was supposed to fail when mixin forwarders were generated before erasure,
// but didn't due to separate compilation unlike mixin-forwarder-clash1,
// it's not supposed to fail anymore since the forwarders generated after erasure do not clash,
// the comments are preserved for posterity.

class Bar2 extends Bar1 with Two[Foo] // error
  // We get a mixin forwarder for Two:
  //   override def concat[Dummy](suffix: Int): Foo
  // which gets erased to:
  //   override def concat(suffix: Int): Foo
  // This clashes with the forwarder generated in Bar1, but
  // unlike with mixin-forwarder-clash1, the compiler
  // doesn't detect that due to separate compilation,
  // so this test case fails.
