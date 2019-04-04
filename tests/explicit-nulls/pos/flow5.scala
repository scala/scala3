// Test that flow inference handles `eq/ne`.

class Test {
  val x: String|Null = ???

  if (!(x eq null)) {
    val y = x.length
  }

  if (x eq null) {
  } else {
    val y = x.length
  }

  if (x ne null) {
    val y = x.length
  }
}

