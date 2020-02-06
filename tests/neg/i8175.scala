opaque type A = String
val x: A = "abc"

object obj {
  val y: A = "abc"  // error: cannot assign "abc" to opaque type A
}