def posId(x: Int with x >= 0): {r: Int with x == r} =
  x

def test(size: Int): Unit =
  posId(size.runtimeChecked)
