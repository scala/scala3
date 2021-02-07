def f(xs: Int*) = xs.sum
def test =
  f(List(1, 2, 3) *)

def g = { implicit x: Int =>
  x + 1
}