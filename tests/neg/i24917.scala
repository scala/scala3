val ab: (a: Int, b: String) = (42, "hello, world")

def `named tuple type in pattern with binding` =
  ab match
  case x: (y: Int, z: String) => x.z * x.y // error
