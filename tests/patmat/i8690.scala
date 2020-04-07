class A
class B

def test(x: (A, B) | (B, A)) = x match {
  case (u: A, v) => (u, v)
  case (u: B, v) => (v, u)
}
