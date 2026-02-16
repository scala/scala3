class Foo:
  given ord: Ordering[Int] = summon[Ordering[Int]]
