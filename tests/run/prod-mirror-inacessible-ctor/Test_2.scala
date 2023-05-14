@main def Test =
  assert(lib.CallSiteSucceed.mFoo eq lib.Foo) // binary compatibility with 3.1
  assert(lib.CallSiteSucceed.mBar ne lib.Bar) // anonymous mirror

  assert(lib.CallSiteSucceed.mFoo.ordinal(lib.CallSiteSucceed.sampleBar) == 0)
  assert(lib.CallSiteSucceed.mBar.fromProduct(EmptyTuple) == lib.CallSiteSucceed.sampleBar)
