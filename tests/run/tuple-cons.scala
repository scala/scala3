
object Test extends App {
  val emptyTuple: Tuple = Tuple()
  val tuple: Tuple = ("1", "2", "3", "4", "5")
  val tupleXXL: Tuple = ("11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31", "32", "33", "34", "35")
  val tuple22: Tuple = ("36", "37", "38", "39", "40", "41", "42", "43", "44", "45", "46", "47", "48", "49", "50", "51", "52", "53", "54", "55", "56", "57")

  // Test all possible combinations of making
  println("head" *: emptyTuple)
  println("head" *: tuple)
  println("head" *: tupleXXL)
  println("head" *: tuple22)
}
