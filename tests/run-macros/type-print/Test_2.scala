package a {
  infix type Op[A, B]
}

inline def printAll[T]: Unit =
  println(printTypeShort[T])
  println(printType[T])
  println(printTypeAnsi[T])
  println(printTypeStructure[T])

@main
def Test: Unit =
  printAll[List[Int]]
  import a.Op
  printAll[Int Op String]
