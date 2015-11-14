object Test {
  final val x = 2
  final val y = { println("x"); 2 }
  val x1 = x
  val y1 = y
  object O { val x = 42 }
  println(O.x)
}
