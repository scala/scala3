object Test extends App {

  class R {
    def _eq(that: R | N): Boolean = this eq that
  }
  class N
  object N extends N

  extension (x: N) def _eq(y: R | N) = y eq N

  val r1, r2 = new R
  assert(r1 _eq r1)
  assert(!(r1 _eq r2))
  assert(!(r1 _eq N))
  assert(!(N _eq r1))
  assert(N _eq N)
}