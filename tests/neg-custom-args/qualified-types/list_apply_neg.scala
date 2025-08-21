type PosInt = {v: Int with v > 0}

@main def Test =
  val l: List[PosInt] = List(1,-2,3) // error // error
