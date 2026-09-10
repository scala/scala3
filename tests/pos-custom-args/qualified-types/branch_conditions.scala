// Test that branch conditions are available as assumptions in the qualifier solver.
def test(x: Int, y: Int): {res: Int with res == x} =
  if x == y then y
  else x
