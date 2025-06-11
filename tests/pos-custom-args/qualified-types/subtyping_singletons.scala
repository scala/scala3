type Pos = {v: Int with v > 0}

def test: Unit =
  val x: Int = ???
  val one: Int = 1
  summon[1 <:< {v: Int with v == 1}]
  summon[1 <:< {v: Int with v > 0}]
  summon[1 <:< Pos]
  summon[x.type <:< {v: Int with v == x}]
  summon[one.type <:< {v: Int with v == 1}]
