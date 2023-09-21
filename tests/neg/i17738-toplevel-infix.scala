//> using options -explain
infix type A[b, a] = Nothing

infix type B[b, a] = b match {
  case Int => a
}

infix class C[A, B]
infix trait D[A, B]

extension (x: Boolean)
  infix def or (y: => Boolean) = x || y

infix val toplevelVal = ??? // error
infix var toplevelVar = ??? // error
infix def toplevelDef = ??? // error
infix given toplevelGiven: Int = ??? // error
infix implicit val toplevelImplicit: Int = ??? // error
