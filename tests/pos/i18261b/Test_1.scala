def baz[L](lhs: L)(using icL: Candidate[L]): DFBits[Int] = ???
object Test:
  val x: DFBits[8] = ???
  val z: DFBits[Int] = baz(x)
  summon[Candidate[z.type]]
