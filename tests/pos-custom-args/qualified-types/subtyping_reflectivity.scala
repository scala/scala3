@main def Test =
  val n: Int = ???
  summon[{v: Int with v == 2} <:< {v: Int with v == 2}]
  summon[{v: Int with v == n} <:< {v: Int with v == n}]
