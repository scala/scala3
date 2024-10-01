//> using options -experimental

@main def Test =

  // ensure that impl.foo isn't `Invisible`, so can be resolved from TASTy
  assert(UnrolledImpl.impl.foo("foo") == "foo1true")
