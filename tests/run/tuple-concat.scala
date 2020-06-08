
object Test extends App {
  val emptyTuple: Tuple = Tuple()
  val tuple1: Tuple = ("1", "2", "3", "4", "5")
  val tuple2: Tuple = ("6", "7", "8", "9", "10")
  val tupleXXL1: Tuple = ("11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31", "32", "33", "34", "35")
  val tupleXXL2: Tuple = ("36", "37", "38", "39", "40", "41", "42", "43", "44", "45", "46", "47", "48", "49", "50", "51", "52", "53", "54", "55", "56", "57", "58", "59", "60")

  // All possible combinations of concatenating two tuples
  println(emptyTuple ++ emptyTuple)
  println(emptyTuple ++ tuple1)
  println(tuple1 ++ emptyTuple)
  println(tupleXXL1 ++ emptyTuple)
  println(emptyTuple ++ tupleXXL1)
  println(tuple1 ++ tuple2)
  println(tuple1 ++ tupleXXL1)
  println(tupleXXL1 ++ tuple1)
  println(tupleXXL1 ++ tupleXXL2)

  // Concatenation with an empty tuple should be a no-op
  assert((tuple1 ++ emptyTuple).asInstanceOf[AnyRef] eq tuple1.asInstanceOf[AnyRef])
  assert((tupleXXL1 ++ emptyTuple).asInstanceOf[AnyRef] eq tupleXXL1.asInstanceOf[AnyRef])
  assert((emptyTuple ++tuple1).asInstanceOf[AnyRef] eq tuple1.asInstanceOf[AnyRef])
  assert((emptyTuple ++ tupleXXL1).asInstanceOf[AnyRef] eq tupleXXL1.asInstanceOf[AnyRef])
}
