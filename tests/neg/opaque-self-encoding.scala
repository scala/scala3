trait TT { self: { type TX = Int } =>
  type TX
  def lift(x: Int): TX = x
}

class Test {
  val t = new TT {}
  t.lift(1): Int // error
}
