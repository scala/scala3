object o {
  opaque type T = String

  summon[o.T =:= T]      // OK
  summon[o.T =:= String] // OK

  def test1(t: T): Int =
    t.length // OK

  def test2(t: o.T): Int =
    t.length // OK
}
