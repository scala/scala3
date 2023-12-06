//> using options -Werror

class Test:
  val (_, (
    _, _, _, _, _, _, _, _, _, _, // 10
    _, _, _, _, _, _, _, _, _, _, // 20
    _, c22, _                     // 23
  )) = // nested pattern has 23 elems
    (0, (
      1, 2, 3, 4, 5, 6, 7, 8, 9, 10,
      1, 2, 3, 4, 5, 6, 7, 8, 9, 20,
      1, 2, 3
    )) // ok, exhaustive, reachable, conforming and irrefutable
