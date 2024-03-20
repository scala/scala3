import scala.math.Ordering.Implicits.infixOrderingOps

def test =
  if (1, 2) < (3, 4) then 1 else 2
