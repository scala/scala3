@main def Test =
  val foo = summon[Foo[Box]].foo
  assert(foo == 1, foo)
