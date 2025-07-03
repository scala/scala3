def size(v: Vec): Int = ???
type Vec

def vec(s: Int): {v: Vec with size(v) == s} = ???
def concat(v1: Vec, v2: Vec): {v: Vec with size(v) == size(v1) + size(v2)} = ???
def sum(v1: Vec, v2: Vec with size(v1) == size(v2)): {v: Vec with size(v) == size(v1)} = ???

@main def Test =

  val v3: {v: Vec with size(v) == 3} = vec(3)
  val v4: {v: Vec with size(v) == 4} = vec(4)
  /*
  val v7: {v: Vec with size(v) == 7} = concat(v3, v4)
  */
  // TODO(mbovel): need constraints of referred term refs
