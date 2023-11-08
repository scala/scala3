object A {             
  val a: Int = B.b
}

object B {
  val b: Int = A.a     
}

@main
def Test = print(A.a)

// nopos-error: No warnings can be incurred under -Werror.