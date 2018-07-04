object DepNats {
  sealed trait Nat { val pred: Nat }
  case object Zero extends Nat { val pred: Nat = Zero }
  /*transparent*/ case class Succ(pred: Nat) extends Nat

  transparent def asNat(i: Int): Nat =
    if (i == 0) Zero
    else        Succ(asNat(i - 1))

  val Nat0: {Zero}             = asNat(0)
  val Nat1: {Succ(Zero)}       = asNat(1)
  val Nat2: {Succ(Succ(Zero))} = asNat(2)

  // Succ(Zero).pred: {Zero}
  // Nat1.pred: {Zero}
  // val _: {Nat1} = Nat2.pred  // FIXME: Why is `Nat2.pred` considered a pure expression but the above are not?

  // transparent def isZero(n: Nat): Boolean =
  //   n.isInstanceOf[{Zero}]

  // implicitly[{isZero(Nat0)} =:= true]
  // val Nat1IsNotZero: false = isZero(Nat1)

  // transparent def plus(n: Nat, m: Nat): Nat =
  //   if (isZero(m)) n
  //   else           plus(Succ(n), m.pred)

  // plus(Zero, Zero): {Zero}
  // plus(Succ(Zero), Zero): {Succ(Zero)}
  // plus(Zero, Succ(Zero)): {Succ(Zero)}
  // plus(Nat1, Nat1): {Nat2}
}