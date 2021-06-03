case class A(i: Int)
case class B(b: Boolean)
case class C(s: String)
case class D(c: C)
case class E(i: Int)
case class F(i: Int, e: E)
case class G(i: Int)
case class H(i: Int, e: G)

def Test =
  val (x, given A) = (1, A(23)) // error
  val (_, given B) = (true, B(false)) // error
  val D(given C) = D(C("c")) // error
  val F(y, given E) = F(47, E(93)) // error
  val H(z, q @ given G) = H(47, G(101)) // error
  assert(summon[A] == A(23)) // error
  assert(summon[B] == B(false)) // error
  assert(summon[C] == C("c")) // error
  assert(summon[E] == E(93)) // error
  assert(summon[G] == G(101)) // error
