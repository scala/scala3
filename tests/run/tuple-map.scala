
object Test extends App {
  val emptyTuple: Tuple = Tuple()
  val tuple: Tuple = ("1", "2", "3", "4", "5")
  val tupleXXL: Tuple = ("11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31", "32", "33", "34", "35")

  type Id[X] = X
  val f: [t] => t => Id[t] = [t] => (x: t) => {
    val str = x.asInstanceOf[String]
    str.updated(0, (str(0) + 1).toChar).asInstanceOf[t]
  }

  // Test all possible combinations of making
  println(emptyTuple.map(f))
  println(tuple.map(f))
  println(tupleXXL.map(f))
}
