enum Nat:
  case Zero
  case Succ[N <: Nat](n: N)
