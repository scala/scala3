import caps.fresh
class C
type Cap = C^

class S

def f(y: Cap) =
  val a: ((x: Cap) -> S^{fresh}) = (x: Cap) => S()