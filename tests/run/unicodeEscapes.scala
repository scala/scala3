object Test {
  def test(prelude: String, assertion: => Boolean): Unit = {
    print(s"$prelude...")
    assert(assertion)
    print("OK")
    print("\n")
  }
  def main(args: Array[String]): Unit = {
    test("chars", 'a' == '\u0061')
    test("char delim", '\'' == '\u0027')
    test("string", "abcd" == "\u0061b\u0063d")
    test("string delim", "\"" == "\u0022")
    val `id\u0061` = 17
    test("back-quoted identifier", ida == 17)
    val abcescape = List('\\', 'u', '0', '0', '6', '1', 'b', 'c')
    test("""triple quoted string""", """\u0061bc""".toList == abcescape)
    val b = 'b'
    test("single quoted s interpolation", "abc" == s"\u0061${b}c")
    test("triple quoted s interpolation", "abc" == s"""\u0061${b}c""")
    //test("f interpolation", "abc" == f"\u0061${b}c")
    //raw is *NOT* processed (as it shouldn't be)
    test("single quoted raw interpolation", raw"\u0061${b}c".toList == abcescape)
    test("triple quoted raw interpolation", raw"""\u0061${b}c""".toList == abcescape)
  }
}
