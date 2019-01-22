
// Test that flow inference handles `isInstanceOf` checks.
class Foo {
  val x: String|Null = ""
  if (!x.isInstanceOf[Null]) {
    val y = x.length
  }

  if (x.isInstanceOf[Null]) {
  } else {
    val y = x.length
  }

  if (x.isInstanceOf[String]) {
    val y = x.length // error: we only infer if the type argument is `Null`
  }
}
