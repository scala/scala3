// Basic block avoidance: local val referenced in qualifier
def test1 =
  val x =
    val y: Int = ???
    y: {v: Int with v == y}

// Unstable arg lifted to val, result qualifier references it
def id(x: Int): {r: Int with r == x} = x.runtimeChecked
def test2 =
  val a: Int = id(1 + 1)

// Multiple unstable args, result depends on both
def add(x: Int, y: Int): {r: Int with r == x + y} = (x + y).runtimeChecked
def test3 =
  val b: Int = add(1 + 1, 2 + 2)

// Unstable arg with param-dependent qualified type
def dep(x: Int, y: {v: Int with v > x}): {r: Int with r == y} = y.runtimeChecked
def test4 =
  def getInt(): Int = 42
  val c = dep(getInt(), 1.runtimeChecked)
