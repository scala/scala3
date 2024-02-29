class Church[B]:
  type Nat = Tuple1[B]

class Test:
  given makeChurch[C]: Church[C] = ??? // necessary to cause crash

  def churchTest(c: Church[Int]): Unit =
    val res1 = summon[c.Nat =:= Int] // error (not a compiler crash)
