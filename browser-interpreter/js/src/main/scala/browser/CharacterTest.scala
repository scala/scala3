package browser

import scala.scalajs.js
import scala.scalajs.js.annotation._

/**
 * Test harness for verifying java.lang.Character methods work in Scala.js.
 * This is critical for the Scanner to work.
 */
@JSExportTopLevel("CharacterTest")
object CharacterTest {

  @JSExport
  def testAll(): String = {
    val sb = new StringBuilder
    sb.append("=== Character Method Tests ===\n\n")

    // Basic ASCII tests
    sb.append("--- ASCII Letters ---\n")
    sb.append(s"isLetter('a') = ${Character.isLetter('a')} (expected: true)\n")
    sb.append(s"isLetter('Z') = ${Character.isLetter('Z')} (expected: true)\n")
    sb.append(s"isLetter('5') = ${Character.isLetter('5')} (expected: false)\n")

    sb.append("\n--- ASCII Digits ---\n")
    sb.append(s"isDigit('0') = ${Character.isDigit('0')} (expected: true)\n")
    sb.append(s"isDigit('9') = ${Character.isDigit('9')} (expected: true)\n")
    sb.append(s"isDigit('a') = ${Character.isDigit('a')} (expected: false)\n")

    // Unicode identifier tests (critical for Scanner)
    sb.append("\n--- Unicode Identifier Start ---\n")
    sb.append(s"isUnicodeIdentifierStart('a') = ${Character.isUnicodeIdentifierStart('a')} (expected: true)\n")
    sb.append(s"isUnicodeIdentifierStart('_') = ${Character.isUnicodeIdentifierStart('_')} (expected: false)\n")
    sb.append(s"isUnicodeIdentifierStart('α') = ${Character.isUnicodeIdentifierStart('α')} (expected: true)\n")
    sb.append(s"isUnicodeIdentifierStart('中') = ${Character.isUnicodeIdentifierStart('中')} (expected: true)\n")
    sb.append(s"isUnicodeIdentifierStart('5') = ${Character.isUnicodeIdentifierStart('5')} (expected: false)\n")
    sb.append(s"isUnicodeIdentifierStart('$$') = ${Character.isUnicodeIdentifierStart('$')} (expected: false)\n")

    sb.append("\n--- Unicode Identifier Part ---\n")
    sb.append(s"isUnicodeIdentifierPart('a') = ${Character.isUnicodeIdentifierPart('a')} (expected: true)\n")
    sb.append(s"isUnicodeIdentifierPart('5') = ${Character.isUnicodeIdentifierPart('5')} (expected: true)\n")
    sb.append(s"isUnicodeIdentifierPart('_') = ${Character.isUnicodeIdentifierPart('_')} (expected: true)\n")
    sb.append(s"isUnicodeIdentifierPart(' ') = ${Character.isUnicodeIdentifierPart(' ')} (expected: false)\n")

    // Surrogate pair tests (for emoji and supplementary characters)
    sb.append("\n--- Surrogate Pairs ---\n")
    sb.append(s"isHighSurrogate(0xD800) = ${Character.isHighSurrogate(0xD800.toChar)} (expected: true)\n")
    sb.append(s"isHighSurrogate(0xDBFF) = ${Character.isHighSurrogate(0xDBFF.toChar)} (expected: true)\n")
    sb.append(s"isHighSurrogate('a') = ${Character.isHighSurrogate('a')} (expected: false)\n")
    sb.append(s"isLowSurrogate(0xDC00) = ${Character.isLowSurrogate(0xDC00.toChar)} (expected: true)\n")
    sb.append(s"isLowSurrogate(0xDFFF) = ${Character.isLowSurrogate(0xDFFF.toChar)} (expected: true)\n")

    // Code point conversion
    sb.append("\n--- Code Point Conversion ---\n")
    val high = 0xD83D.toChar  // Part of emoji 😀
    val low = 0xDE00.toChar
    val codePoint = Character.toCodePoint(high, low)
    sb.append(s"toCodePoint(0xD83D, 0xDE00) = ${codePoint} (expected: 128512)\n")
    sb.append(s"isValidCodePoint(${codePoint}) = ${Character.isValidCodePoint(codePoint)} (expected: true)\n")

    // Whitespace
    sb.append("\n--- Whitespace ---\n")
    sb.append(s"isWhitespace(' ') = ${Character.isWhitespace(' ')} (expected: true)\n")
    sb.append(s"isWhitespace('\\t') = ${Character.isWhitespace('\t')} (expected: true)\n")
    sb.append(s"isWhitespace('\\n') = ${Character.isWhitespace('\n')} (expected: true)\n")
    sb.append(s"isWhitespace('a') = ${Character.isWhitespace('a')} (expected: false)\n")

    sb.toString
  }

  @JSExport
  def runValidation(): js.Dynamic = {
    var passed = 0
    var failed = 0
    val failures = new scala.collection.mutable.ListBuffer[String]()

    def check(name: String, actual: Boolean, expected: Boolean): Unit = {
      if (actual == expected) {
        passed += 1
      } else {
        failed += 1
        failures += s"$name: got $actual, expected $expected"
      }
    }

    // Critical tests for Scanner
    check("isLetter('a')", Character.isLetter('a'), true)
    check("isLetter('5')", Character.isLetter('5'), false)
    check("isDigit('0')", Character.isDigit('0'), true)
    check("isDigit('a')", Character.isDigit('a'), false)
    check("isUnicodeIdentifierStart('a')", Character.isUnicodeIdentifierStart('a'), true)
    check("isUnicodeIdentifierStart('α')", Character.isUnicodeIdentifierStart('α'), true)
    check("isUnicodeIdentifierStart('5')", Character.isUnicodeIdentifierStart('5'), false)
    check("isUnicodeIdentifierPart('a')", Character.isUnicodeIdentifierPart('a'), true)
    check("isUnicodeIdentifierPart('5')", Character.isUnicodeIdentifierPart('5'), true)
    check("isUnicodeIdentifierPart(' ')", Character.isUnicodeIdentifierPart(' '), false)
    check("isHighSurrogate(0xD800)", Character.isHighSurrogate(0xD800.toChar), true)
    check("isLowSurrogate(0xDC00)", Character.isLowSurrogate(0xDC00.toChar), true)
    check("isWhitespace(' ')", Character.isWhitespace(' '), true)
    check("isWhitespace('a')", Character.isWhitespace('a'), false)

    js.Dynamic.literal(
      passed = passed,
      failed = failed,
      total = passed + failed,
      success = failed == 0,
      failures = js.Array(failures.toSeq.map(js.Any.fromString)*)
    )
  }
}

