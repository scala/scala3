type PosInt = {v: Int with v > 0}


def inc(x: PosInt): PosInt = (x + 1).runtimeChecked

@main def Test =
  val l: List[PosInt] = List(1,2,3)
  val l2: List[PosInt] = l.map(inc)
  ()
