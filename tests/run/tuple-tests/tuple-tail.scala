
def tail(): Unit = {
  val singletonTuple: NonEmptyTuple = Tuple1("59")
  val tuple: NonEmptyTuple = ("1", "2", "3", "4", "5")
  val tupleXXL: NonEmptyTuple = ("11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31", "32", "33", "34", "35")
  val tuple23: NonEmptyTuple = ("36", "37", "38", "39", "40", "41", "42", "43", "44", "45", "46", "47", "48", "49", "50", "51", "52", "53", "54", "55", "56", "57", "58")

  // Test all possible combinations of making
  println(singletonTuple.tail)
  println(tuple.tail)
  println(tupleXXL.tail)
  println(tuple23.tail)
}
