// Test that flow inference can handle blocks.

class Foo {
  val x: String|Null = "hello"
  if ({val z = 10; {1 + 1 == 2; x != null}}) {
    val l = x.length
  }

  if ({x != null; true}) {
    val l = x.length // error
  }

  val x2: String|Null = "world"
  if ({{{{1 + 1 == 2; x != null}}}} && x2 != null) {
    val l = x.length
    val l2 = x2.length
  }
}
