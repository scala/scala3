
object Test extends App {
  private def dump(s: String) = s.map(c => f"${c.toInt}%02X").mkString(" ")
  def assertEqualStrings(expected: String)(actual: String) =
    assert(expected == actual, s"Expected:\n${dump(expected)}\nActual:\n${dump(actual)}")
  val c = new C_1
  assert(C_1.NULLED.length == "XYABC".length)
  assert(C_1.SUPPED.codePointCount(0, C_1.SUPPED.length) == 8)

  assertEqualStrings(c.nulled)("X\u0000ABC")    // "X\000ABC" in java source
  assertEqualStrings(c.supped)("𐒈𐒝𐒑𐒛𐒐𐒘𐒕𐒖")

  assertEqualStrings(C_1.NULLED)("X\u0000ABC")  // "X\000ABC" in java source
  assertEqualStrings(C_1.SUPPED)("𐒈𐒝𐒑𐒛𐒐𐒘𐒕𐒖")
}
