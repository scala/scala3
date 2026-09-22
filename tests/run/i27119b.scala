//> using options -language:experimental.genericNumberLiterals

@main def Test: Unit =
  val n: Long = 0xffffffff
  assert(n > 0)
