object Test {
  val a: 42 = 43  // error: different constant
  val x = 42
  val z: 42 = x   // error: x is not final
}
