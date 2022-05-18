
package dotty.tools
package dotc.util

import Spans.*

import org.junit.Assert.{assertThrows as _, *}
import org.junit.Test

class SourceFileTests:
  @Test def `i15209 source column handles tabs as line index`: Unit =
    val text = "\ta\n \tb\n \t c\n"
    val f = SourceFile.virtual("batch", text)
    assertEquals(1, f.column(text.indexOf('a')))
    assertEquals(2, f.column(text.indexOf('b')))
    assertEquals(3, f.column(text.indexOf('c')))

  def lineContentOf(code: String, offset: Int) = SourceFile.virtual("batch", code).lineContent(offset)

  @Test def si8205_lineToString: Unit =
    assertEquals("", lineContentOf("", 0))
    assertEquals("abc", lineContentOf("abc", 0))
    assertEquals("abc", lineContentOf("abc", 3))
    assertEquals("code no newline", lineContentOf("code no newline", 1))
    assertEquals("\n", lineContentOf("\n", 0))
    assertEquals("abc\n", lineContentOf("abc\ndef", 0))
    assertEquals("abc\n", lineContentOf("abc\ndef", 3))
    assertEquals("def", lineContentOf("abc\ndef", 4))
    assertEquals("def", lineContentOf("abc\ndef", 6))
    assertEquals("def\n", lineContentOf("abc\ndef\n", 7))

  @Test def CRisEOL: Unit =
    assertEquals("\r", lineContentOf("\r", 0))
    assertEquals("abc\r", lineContentOf("abc\rdef", 0))
    assertEquals("abc\r", lineContentOf("abc\rdef", 3))
    assertEquals("def", lineContentOf("abc\rdef", 4))
    assertEquals("def", lineContentOf("abc\rdef", 6))
    assertEquals("def\r", lineContentOf("abc\rdef\r", 7))

  @Test def CRNLisEOL(): Unit =
    assertEquals("\r\n", lineContentOf("\r\n", 0))
    assertEquals("abc\r\n", lineContentOf("abc\r\ndef", 0))
    assertEquals("abc\r\n", lineContentOf("abc\r\ndef", 3))
    assertEquals("abc\r\n", lineContentOf("abc\r\ndef", 4))
    assertEquals("def", lineContentOf("abc\r\ndef", 5))
    assertEquals("def", lineContentOf("abc\r\ndef", 7))
    assertEquals("def", lineContentOf("abc\r\ndef", 8))
    assertEquals("def\r\n", lineContentOf("abc\r\ndef\r\n", 9))

  @Test def `t9885 lineToOffset throws on bad line`: Unit =
    val text = "a\nb\nc\n"
    val f = SourceFile.virtual("batch", text)
    // was: EOL is line terminator, not line separator, so there is not an empty 4th line
    val splitsville = text.split("\n")
    assertEquals(List(0, 2, 4, 6), (0 to splitsville.nn.length).toList.map(f.lineToOffset))
    assertThrows[IndexOutOfBoundsException] {
      f.lineToOffset(4)   // was: 3 in Scala 2 (no empty line), 5 in Scala 3 (sentinel induces off-by-one)
    }

    // Position and SourceFile used to count differently
    val p = SourcePosition(f, Span(text.length - 1))
    val q = SourcePosition(f, Span(f.lineToOffset(p.line - 1)))
    assertEquals(2, p.line)
    assertEquals(p.line - 1, q.line)
    assertEquals(p.column, q.column + 1)
    assertEquals(f.startOfLine(p.span.start), SourcePosition(f, Span(f.lineToOffset(p.line))).span.start)

  @Test def `t9885 lineToOffset ignores lack of final EOL`: Unit =
    val text = "a\nb\nc"
    val f = SourceFile.virtual("batch", text)
    assertThrows[IndexOutOfBoundsException] {
      f.lineToOffset(3)
    }
    assertEquals(4, f.lineToOffset(2))
    assertEquals(2, f.offsetToLine(text.length))

  @Test def `t11572 offsetToLine throws on bad offset`: Unit =
    val text = "a\nb\nc\n"
    val f = SourceFile.virtual("batch", text)
    /* current code requires offsets untethered from source
    assertThrows[IndexOutOfBoundsException] {
      f.offsetToLine(-1)  // was: -1
    }
    assertThrows[IndexOutOfBoundsException] {
      f.offsetToLine(7)   // was: 3
    }
    */
    assertEquals(0, f.offsetToLine(0))
    assertEquals(0, f.offsetToLine(1))
    assertEquals(1, f.offsetToLine(2))
    assertEquals(2, f.offsetToLine(4))
    assertEquals(2, f.offsetToLine(5))
    assertEquals(3, f.offsetToLine(6))

  @Test def `t11572b offsetToLine throws on bad offset`: Unit =
    val text = "a\nb\nc\nd"
    val f = SourceFile.virtual("batch", text)
    /*
    assertThrows[IndexOutOfBoundsException] {
      f.offsetToLine(-1)
    }
    assertThrows[IndexOutOfBoundsException] {
      f.offsetToLine(8)
    }
    */
    assertEquals(0, f.offsetToLine(0))
    assertEquals(0, f.offsetToLine(1))
    assertEquals(1, f.offsetToLine(2))
    assertEquals(2, f.offsetToLine(4))
    assertEquals(2, f.offsetToLine(5))
    assertEquals(3, f.offsetToLine(6))
    assertEquals(3, f.offsetToLine(7))
