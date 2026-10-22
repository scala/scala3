import language.experimental.genericNumberLiterals

@main def Test: Unit =
  val m: Long = 0xffffffff // warn
  val n: Long = 0xffffffffL // explicit
