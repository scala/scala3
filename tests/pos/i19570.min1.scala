enum Op[A]:
  case Dup[T]() extends Op[(T, T)]

def foo[R](f: [A] => Op[A] => R): R = ???

def test =
  foo([A] => (o: Op[A]) => o match
    case o: Op.Dup[u] =>
      summon[A =:= (u, u)] // Error: Cannot prove that A =:= (u, u)
      ()
  )
  foo[Unit]([A] => (o: Op[A]) => o match
    case o: Op.Dup[u] =>
      summon[A =:= (u, u)] // Ok
      ()
  )
  foo({
    val f1 = [B] => (o: Op[B]) => o match
      case o: Op.Dup[u] =>
        summon[B =:= (u, u)] // Also ok
        ()
    f1
  })
