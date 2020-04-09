class A
class B

def test(x: (A, B) | (B, A)) = x match {
  case (u: A, v: B) => (u, v)
  case (u: B, v: A) => (v, u)
}
