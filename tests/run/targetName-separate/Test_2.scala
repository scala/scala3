@main def Test =
  assert(new Foo().it == 42)
  assert(Foo().it == 42)
