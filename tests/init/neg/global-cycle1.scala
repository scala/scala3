object A {
  val a: Int = B.b     // error
}

object B {
  val b: Int = A.a     // error
}

@main
def Test = print(A.a)