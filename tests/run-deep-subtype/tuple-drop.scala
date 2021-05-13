
object Test extends App {
  val emptyTuple: Tuple = Tuple()
  val tuple: Tuple = ("1", "2", "3", "4", "5")
  val tupleXXL: Tuple = ("11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31", "32", "33", "34", "35")

  println(emptyTuple.drop(0))
  println(emptyTuple.drop(1))
  println(emptyTuple.drop(10))

  println(tuple.drop(0))
  println(tuple.drop(1))
  println(tuple.drop(3))
  println(tuple.drop(5))
  println(tuple.drop(10))

  println(tupleXXL.drop(0))
  println(tupleXXL.drop(1))
  println(tupleXXL.drop(10))
  println(tupleXXL.drop(20))
  println(tupleXXL.drop(25))
  println(tupleXXL.drop(30))
}
