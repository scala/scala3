
trait t6810 {
  val x = '\u000A'    // char literals accept arbitrary unicode escapes
                      // anypos-error so as not to interfere with the following bad syntax
  val y = '
'                     // but not embedded EOL sequences not represented as escapes
  println();          // scanner firewall
  val z = '\n'        // normally, expect this escape

  val X = "\u000A"    // it's the same as ordinary string literals
                      // anypos-error so as not to interfere with the following bad syntax
  val Y = "
"                     // error obviously not
  val Z = "\n"        // normally, expect this escape

  val A = """
"""                   // which is what these are for
  val B = s"""
"""                   // or the same for interpolated strings

  val `\u000A` = "\n" // backquoted identifiers are arbitrary string literals
                      // anypos-error so as not to interfere with the following bad syntax
  val `
` = "\n"              // error not raw string literals aka triple-quoted, multiline strings

  val firebreak = 42  // help parser recovery, could also use rbrace

  val a = '\u000D'    // similar treatment of CR
  val b = '
'        // anypos-error CR seen as EOL by scanner; FSR, error only on open quote, unlike `y`
  println();          // scanner firewall
  val c = '\r'        // traditionally
}
