
type Rec[X] = X match
  case Int => Rec[X]

type M[Unused, Y] = Y match
  case String => Double

def foo[X](d: M[Rec[X], "hi"]) = ???
