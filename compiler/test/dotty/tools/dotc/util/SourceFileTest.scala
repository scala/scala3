package dotty.tools.dotc.util

import org.junit.Assert.*
import org.junit.Test

// Partly ported from scala/scala's test/junit/scala/reflect/internal/util/SourceFileTest.scala,
// adapted to dotc's conventions: `lineContent` includes the line terminator, and
// `offsetToLine` maps offsets at or past the end to the last line.
class SourceFileTest {
  private def source(code: String) = SourceFile.virtual("test", code)

  private def lineContentOf(code: String, offset: Int) = source(code).lineContent(offset)

  @Test def lineContent(): Unit = {
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
  }

  @Test def CRisEOL(): Unit = {
    assertEquals("\r", lineContentOf("\r", 0))
    assertEquals("abc\r", lineContentOf("abc\rdef", 0))
    assertEquals("abc\r", lineContentOf("abc\rdef", 3))
    assertEquals("def", lineContentOf("abc\rdef", 4))
    assertEquals("def", lineContentOf("abc\rdef", 6))
    assertEquals("def\r", lineContentOf("abc\rdef\r", 7))
  }

  @Test def CRLFisEOL(): Unit = {
    assertEquals("\r\n", lineContentOf("\r\n", 0))
    assertEquals("abc\r\n", lineContentOf("abc\r\ndef", 0))
    assertEquals("abc\r\n", lineContentOf("abc\r\ndef", 3))
    assertEquals("abc\r\n", lineContentOf("abc\r\ndef", 4))
    assertEquals("def", lineContentOf("abc\r\ndef", 5))
    assertEquals("def", lineContentOf("abc\r\ndef", 7))
    assertEquals("def", lineContentOf("abc\r\ndef", 8))
    assertEquals("def\r\n", lineContentOf("abc\r\ndef\r\n", 9))
  }

  @Test def FFisEOL(): Unit = {
    val f = source("abc\fdef")
    assertEquals(1, f.offsetToLine(4))
    assertEquals(4, f.lineToOffset(1))
  }

  @Test def lineToOffset(): Unit = {
    val f = source("a\nb\nc\n")
    assertEquals(0, f.lineToOffset(0))
    assertEquals(2, f.lineToOffset(1))
    assertEquals(4, f.lineToOffset(2))
    assertEquals(6, f.lineToOffset(3)) // sentinel
    assertEquals(None, f.lineToOffsetOpt(5))
  }

  @Test def offsetToLine(): Unit = {
    val text = "a\nb\nc"
    val f = source(text)
    assertEquals(0, f.offsetToLine(0))
    assertEquals(0, f.offsetToLine(1))
    assertEquals(1, f.offsetToLine(2))
    assertEquals(2, f.offsetToLine(4))
    assertEquals(2, f.offsetToLine(text.length))
  }

  @Test def manyLines(): Unit = {
    // more lines than the initial size hint of the line index builder
    val n = 1000
    val text = "\n" * n
    val f = source(text)
    for i <- 0 to n do
      assertEquals(i, f.lineToOffset(i))
    for i <- 0 until n do
      assertEquals(i, f.offsetToLine(i))
    assertEquals(n, f.offsetToLine(text.length)) // EOF after the final EOL is on an empty last line
  }

  @Test def lineSizesAgreeWithContents(): Unit = {
    val text = "abc\n\ndefg\nh"
    val f = source(text)
    val g = source(text)
    g.setLineIndicesFromLineSizes(text.split("\n", -1).map(_.length))
    for i <- 0 to text.length do
      assertEquals(f.offsetToLine(i), g.offsetToLine(i))
      assertEquals(f.startOfLine(i), g.startOfLine(i))
  }
}
