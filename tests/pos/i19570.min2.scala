sealed trait Op[A, B]                               { def giveA: A;     def giveB: B }
final case class Dup[T](x: T) extends Op[T, (T, T)] { def giveA: T = x; def giveB: (T, T) = (x, x) }

class Test:
  def foo[R](f: [A, B] => (o: Op[A, B]) => R): R = ???

  def m1: Unit =
    foo([A, B] => (o: Op[A, B]) => o match
      case o: Dup[t] =>
        var a1: t = o.giveA
        var a2: A = o.giveA
        a1 = a2
        a2 = a1

        var b1: (t, t) = o.giveB
        var b2: B      = o.giveB
        b1 = b2
        b2 = b1

        summon[A =:= t]      // ERROR: Cannot prove that A =:= t.
        summon[B =:= (t, t)] // ERROR: Cannot prove that B =:= (t, t).

        ()
    )
