object A {           // error
  val a: Int = B.b
}

object B {          // error
  val b: Int = A.a
}

@main
def Test = print(A.a)