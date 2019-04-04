
// Test that flow inference handles `eq/ne` checks.
class Foo {
  val x: String|Null = ""

  if (x eq null) {
    val y = x.length // error
  } else {
    val y = x.length
  }


  if (x ne null) {
    val y = x.length
  } else {
    val y = x.length // error
  }
}
