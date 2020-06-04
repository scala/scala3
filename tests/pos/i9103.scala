object a:
  type Foo[T]
  given as Foo[Unit] = ???

val b = a

def Test = summon[b.Foo[Unit]]

val n: Long = 1
val total: BigInt = 2
val remainder: BigInt = n % identity(total)
