object Test {
  val a: 42 = 43  // error: different constant
  val x = 42
  val z: 42 = x   // error: x is not final

  val n: null = null // error: Null is not a legal singleton type

  val foo: s"abc" = "abc"  // error: not a legal singleton type
}
