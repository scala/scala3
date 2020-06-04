object a:
  type Foo[T]
  given as Foo[Unit] = ???

val b = a

def Test = summon[b.Foo[Unit]]