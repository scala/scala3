//> using options -language:experimental.modularity -explain

class C[Self]

class D[Self]

trait Test:

  def foo[A: {C, D}] = A // error

  type A: C

  val x = A  // error

  val y: A.type = ??? // error

