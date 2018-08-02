object DepNats {
  sealed trait Nat { val pred: Nat }
  dependent case object Zero extends Nat { val pred: Nat = Zero }
  dependent case class Succ(pred: Nat) extends Nat

  dependent def asNat(i: Int): Nat =
    if (i == 0) Zero
    else        Succ(asNat(i - 1))

  type Nat0 = {Zero}
  type Nat1 = {Succ(Zero)}
  type Nat2 = {Succ(Succ(Zero))}

  val Nat0: Nat0 = asNat(0)
  val Nat1: Nat1 = asNat(1)
  val Nat2: Nat2 = asNat(2)

  dependent def isZero(a: Nat): Boolean =
    a.isInstanceOf[Zero.type]

  dependent def isZeroT[T](a: T): Boolean =
    a.isInstanceOf[Zero.type]

  val v1: true =  isZero(Zero)
  val v2: false = isZero(Succ(Zero))
  val v3: true =  isZeroT(Zero)
  val v4: false = isZeroT(Succ(Zero))

  implicitly[{isZero(Nat0)} =:= true]

  def forward1[T <: Nat](t: T): { t.isInstanceOf[Zero.type] } = isZeroT(t)
  def forward2[T <: Zero.type](t: T): true = isZeroT(t)
  def forward3[T <: Succ](t: T): false = isZeroT(t)

  // val s5: { isZeroT(n) } = forward(Zero)
  // var n: Nat = Zero

  val _0a: {Zero}       = Succ(Zero).pred
  val _0b: {Zero}       = Nat1.pred
  val _1a: {Succ(Zero)} = Nat2.pred
  val _1b: Nat1         = Nat2.pred
//  val _1c: {Nat1}        = Nat2.pred

  dependent def plus(n: Nat, m: Nat): Nat =
    if (isZero(m)) n
    else           plus(Succ(n), m.pred)

  plus(Zero, Zero)       : Nat0
  plus(Succ(Zero), Zero) : Nat1
  plus(Zero, Succ(Zero)) : Nat1
  plus(Nat1, Nat1)       : Nat2
}
