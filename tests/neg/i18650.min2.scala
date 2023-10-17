class Church[B]:
  type Nat = Tuple1[B]

class Test2:
  given makeChurch2[C](using DummyImplicit): Church[C] = ???

  def churchTest2(c: Church[Int]): Unit =
    val res2 = summon[c.Nat =:= Int] // error (not a compiler crash)
