class Foo {
  val a: Int = c    // error
  var b: String = _
  var c: Int = _
  5 + c             // error
}