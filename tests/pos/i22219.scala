type MonthNumber =
  1 | 2

object M:
  val x: MonthNumber = ???
  val number = x
  summon[number.type <:< MonthNumber]
