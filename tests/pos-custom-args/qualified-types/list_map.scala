type Pos = {v: Int with v > 0}

def inc(x: Pos): Pos = (x + 1).runtimeChecked

@main def Test =
  val l: List[Pos] = List(1,2,3)
  val l2 = l.map(inc)
  l2: List[Pos]
  ()
