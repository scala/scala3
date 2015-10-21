// demonstrates selection on non-path types. Needs to be fleshed out to
// become a real test.
object Test {

  class C {
    type T
    val f: T => T = ???
  }

  var x = new C
  val y = x.f


}
