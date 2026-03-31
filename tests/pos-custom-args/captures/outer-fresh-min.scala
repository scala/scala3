import caps.fresh

class A

object Test:

  type F[X] = (t: String) -> X
  type G[cs^] = (t: String) -> A^{cs}

  val x: (s: String) -> F[A^{fresh}] = ???
  val y: (s: String) -> G[{fresh}] = ???

  val z: () -> F[A^{fresh}] = ???
  val z2: () -> G[{fresh}] = ???
