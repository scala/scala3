enum Op[A, B]:
  case Dup[T]() extends Op[T, (T, T)]

def foo[R](f: [A, B] => (o: Op[A, B]) => R): R =
    f(Op.Dup())

def test =
  foo([A, B] => (o: Op[A, B]) => {
    o match
      case o: Op.Dup[t] =>
        summon[A =:= t]      // ERROR: Cannot prove that A =:= t.
        summon[B =:= (t, t)] // ERROR: Cannot prove that B =:= (t, t).
        42
  })
