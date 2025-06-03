object A { // warn
  val a: Int = B.b
}

object B {
  val b: Int = A.a // warn
}

@main
def Test = print(A.a)
