trait A[B[_]]:
  type C

def apply[D[_]](other: (a: A[D]) ?=> a.C): Any = ???

def main =
  apply(()) // error
