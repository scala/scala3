object A {             // error
  val a: Int = B.b
}

object B {
  val b: Int = A.a     // error
}

@main
def Test = print(A.a)

// nopos-error: No warnings can be incurred under -Werror.