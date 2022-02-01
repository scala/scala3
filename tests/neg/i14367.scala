def p(i: Int*) = i.sum
val h2 = i => p(i)  // error: Found (i : Seq[Int]), Required: Int
  // It would be more logical to fail with a "missing parameter type", however.


