import caps.fresh

class A

object Test:

  type F[X] = (t: String) -> X
  type G[cs^] = (t: String) -> A^{cs}

  val x: (s: String) -> F[A^{fresh}] = ???
  val _: (s: String) -> (t: String) -> A = x // error

  val y: (s: String) -> G[{fresh}] = ???
  val _: (s: String) -> (t: String) -> A = y // error

  val z: () -> F[A^{fresh}] = ???
  val _: () -> (t: String) -> A = z // error

  val z2: () -> G[{fresh}] = ???
  val _: () -> (t: String) -> A = z2 // error