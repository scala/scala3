object Bug {
  final val x = 42
  val y = x
  x match {case y.toString => 42 case y => 42}
}
