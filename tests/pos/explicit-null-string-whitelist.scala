// Test that a few commonly-used functions within `java.lang.String` have been whitelisted
// as returning non-nullable values.
class Foo {
  val x1: String = "abc".concat("def")
  val x2: String = "abc".replace("a", "b")
  val x3: String = "abc".replace("a", "b")
  val x4: String = "abc".replace("a", "b")
  val x5: Array[String] = "abc".split(" ", 2)
  val x6: String = "abc".toLowerCase()
  val x7: String = "abc".toUpperCase()
  val x8: String = "abc ".trim()
  val x9: Array[Char] = "abc".toCharArray()
  val x10: String = "abc".substring(1)
}
