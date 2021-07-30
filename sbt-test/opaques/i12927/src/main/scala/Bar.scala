object Bar:
  type Fuzzy[W <: Int] = Int
  opaque type BlaBla[W <: Int] <: Foo.BlaBla[Fuzzy[W], Int] =
    Foo.BlaBla[Fuzzy[W], Int]
