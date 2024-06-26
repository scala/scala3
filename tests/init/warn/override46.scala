trait TA {
  val x = "world"   // warn
}

trait TB { this: TA =>
  val m = "hello" + x
}

class Bar extends TB with TA
