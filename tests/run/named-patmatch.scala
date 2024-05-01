import annotation.experimental
import language.experimental.namedTuples

@main def Test =
  locally:
    val (x = x, y = y) = (x = 11, y = 22)
    assert(x == 11 && y == 22)

  locally:
    val (x = a, y = b) = (x = 1, y = 2)
    assert(a == 1 && b == 2)

  locally:
    val (x = a, y = b) = (x = 1, y = 2)
    assert(a == 1 && b == 2)

  locally:
    val (x, y) = (x = 1, y = 2)
    assert(x == 1 && y == 2)

  locally:
    val (a, b) = (x = 1, y = 2)
    assert(a == 1 && b == 2)

  (x = 1, y = 2) match
    case (x = x, y = y) => assert(x == 1 && y == 2)

  (x = 1, y = 2) match
    case (x, y) => assert(x == 1 && y == 2)

  (x = 1, y = 2) match
    case (a, b) => assert(a == 1 && b == 2)




