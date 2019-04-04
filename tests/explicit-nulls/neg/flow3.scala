
// Test flow typing in the presence of `eq` and `ne`.

class Test {
  val x: String|Null = ???

  if (x eq null) {
    val y = x.length // error
  } else {
    val y = x.length // ok
  }

  if (x ne null) {
    val y = x.length // ok
  } else {
    val y = x.length // error
  }

  if ((x ne null) && x.length > 10) { // ok
  } else {
    val y = x.length // error
  }
}
