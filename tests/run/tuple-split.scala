
object Test extends App {
  val emptyTuple: Tuple = Tuple()
  val tuple: Tuple = ("1", "2", "3", "4", "5")
  val tupleXXL: Tuple = ("11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31", "32", "33", "34", "35")

  println(emptyTuple.splitAt(0))
  println(emptyTuple.splitAt(0))
  println(emptyTuple.splitAt(1))
  println(emptyTuple.splitAt(10))

  println(tuple.splitAt(0))
  println(tuple.splitAt(1))
  println(tuple.splitAt(3))
  println(tuple.splitAt(5))
  println(tuple.splitAt(10))

  println(tupleXXL.splitAt(0))
  println(tupleXXL.splitAt(1))
  println(tupleXXL.splitAt(10))
  println(tupleXXL.splitAt(20))
  println(tupleXXL.splitAt(25))
  println(tupleXXL.splitAt(30))
}
