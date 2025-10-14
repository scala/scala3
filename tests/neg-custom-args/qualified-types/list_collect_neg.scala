type Pos = { v: Int with v >= 0 }

@main def main =
  val xs = List(-1,2,-2,1)
  xs.collect { case x: Int => x } : List[Pos] // error
