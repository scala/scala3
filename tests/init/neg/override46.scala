trait TA {
  val x = "world"   // error
}

trait TB { this: TA =>
  val m = "hello" + x
}

class Bar extends TB with TA
