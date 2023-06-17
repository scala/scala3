infix type A[b, a] = Nothing

infix type B[b, a] = b match {
  case Int => a
}

infix class C[A, B]
infix trait D[A, B]

infix val toplevelVal = ??? // error
infix var toplevelVar = ??? // error
infix def toplevelDef = ??? // error
infix given toplevelGiven: Int = ??? // error
infix implicit val topevelImplicit: Int = ??? // error
