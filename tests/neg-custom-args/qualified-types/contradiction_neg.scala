// Contradictory premises are not exploited (no ex falso): the solver does
// not prove arbitrary goals from them, and must not loop on them either.
def test(x: Int, y: Int): Unit =
  summon[{v: Int with x < y && y < x} <:< {v: Int with x < x}] // error
