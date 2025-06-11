def f(x: Int): Int = ???

def test: Unit =
  val x: Int = ???
  val y: Int = ???
  summon[2 <:< {v: Int with v == 1}] // error
  summon[x.type <:< {v: Int with v == 1}] // error
  //summon[y.type <:< {v: Int with v == x}] // FIXME
