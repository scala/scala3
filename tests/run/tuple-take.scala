
object Test extends App {
  val emptyTuple: Tuple = Tuple()
  val tuple: Tuple = ("1", "2", "3", "4", "5")
  val tupleXXL: Tuple = ("11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31", "32", "33", "34", "35")

  println(emptyTuple.take(0))
  println(emptyTuple.take(0))
  println(emptyTuple.take(1))
  println(emptyTuple.take(10))

  println(tuple.take(0))
  println(tuple.take(1))
  println(tuple.take(3))
  println(tuple.take(5))
  println(tuple.take(10))

  println(tupleXXL.take(0))
  println(tupleXXL.take(1))
  println(tupleXXL.take(10))
  println(tupleXXL.take(20))
  println(tupleXXL.take(25))
  println(tupleXXL.take(30))
}
