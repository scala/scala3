
object Test extends App {
  val emptyTuple: Tuple = ()
  val tuple1: Tuple = "e1" *: "e2" *: "e3" *: ()
  val tuple2: Tuple = "e4" *: "e5" *: "e6" *: ()
  val result: Tuple = "e1" *: "e2" *: "e3" *: "e4" *: "e5" *: "e6" *: ()

  println("tuple1 ++ emptyTuple = " + (tuple1 ++ emptyTuple))
  println("emptyTuple ++ tuple1 = " + (emptyTuple ++ tuple1))
  println("tuple2 ++ emptyTuple = " + (tuple2 ++ emptyTuple))
  println("emptyTuple ++ tuple2 = " + (emptyTuple ++ tuple2))
  println("tuple1 ++ tuple2 = " + (tuple1 ++ tuple2))
  assert((tuple1 ++ emptyTuple).asInstanceOf[AnyRef] eq tuple1.asInstanceOf[AnyRef])
  assert((emptyTuple ++tuple1).asInstanceOf[AnyRef] eq tuple1.asInstanceOf[AnyRef])
}
