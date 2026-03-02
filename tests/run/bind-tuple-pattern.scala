import annotation.experimental

def getNamedTuple: (x: Int, y: String) = (x = 42, y = "Hello")

@main def Test =
  getNamedTuple match
    case (x, y) => assert(x == 42 && y == "Hello")

  getNamedTuple match
    case t @ (x = a, y = b) =>
      // t binds to a named tuple pattern
      // t: (x: Int, y: String)
      assert(a == t.x && b == t.y)

  getNamedTuple match
    case t @ (a, b) =>
      // t binds to a regular tuple pattern
      // t: (Int, String)
      assert(t._1 == a && t._2 == b)