package scala.collection

import org.junit.Test
import org.junit.Assert.assertEquals
import tools.AssertUtil.{assertSameElements, assertThrows}

class StringOpsTest {
  // Test for scala/bug#10951
  @Test def mkstring(): Unit = {
    assert("".mkString("") == "")
    assert("".mkString(",") == "")
    assert("a".mkString(",") == "a")
    assert("ab".mkString(",") == "a,b")
    assert("ab".mkString("foo", ",", "bar") == "fooa,bbar")
  }

  @Test def addString(): Unit = {
    assert("".addString(new StringBuilder).toString == "")
    assert("a".addString(new StringBuilder, ",").toString == "a")
    assert("".addString(new StringBuilder, "foo", ",", "bar").toString == "foobar")
  }

  @Test def toArray(): Unit = {
    assert("".toArray[Any].length == 0) // should not throw
    assertEquals("".mkString(""), "")
    assertEquals("".mkString(","), "")
    assertEquals("a".mkString(","), "a")
    assertEquals("ab".mkString(","), "a,b")
  }
  // Test for scala/bug#8469
  @Test def copyToArray(): Unit = {

      def check(string: String, array: Array[Char], copied: Int, after: Array[Char]): Unit = {
        assertEquals(copied, string.copyToArray(array))
        assert(array.sameElements(after))
      }

      check(string = "", array = Array(), copied = 0, after = Array())
      check(string = "", array = Array('a', 'b'), copied = 0, after = Array('a', 'b'))

      check(string = "abc", array = Array('x', 'y', 'z'), copied = 3, after = Array('a', 'b', 'c'))
      check(string = "abc", array = Array('x'), copied = 1, after = Array('a'))
      check(string = "abc", array = Array(), copied = 0, after = Array())
  }

  // Test for scala/bug#8469
  @Test def copyToArrayStart(): Unit = {
    def check(string: String, start: Int, array: Array[Char], copied: Int, after: Array[Char]): Unit = {
      assertEquals(copied, string.copyToArray(array,start))
      assert(array.sameElements(after))
    }

    check(string = "", start = 0, array = Array(), copied = 0, after = Array())
    check(string = "", start = 1, array = Array(), copied = 0, after = Array())
    check(string = "", start = -1, array = Array(), copied = 0, after = Array())

    assertThrows[IndexOutOfBoundsException](check(string = "abcd", start = -13, array = Array('x', 'y'), copied = 0, after = Array('x', 'y')))
    check(string = "abcd", start = 0, array = Array('x', 'y'), copied = 2, after = Array('a', 'b'))
    check(string = "abcd", start = 1, array = Array('x', 'y'), copied = 1, after = Array('x', 'a'))
    check(string = "abcd", start = 2, array = Array('x', 'y'), copied = 0, after = Array('x', 'y'))
    check(string = "abcd", start = 3, array = Array('x', 'y'), copied = 0, after = Array('x', 'y'))
    check(string = "abcd", start = 4, array = Array('x', 'y'), copied = 0, after = Array('x', 'y'))
    check(string = "abcd", start = 5, array = Array('x', 'y'), copied = 0, after = Array('x', 'y'))
  }

  // Test for scala/bug#8469
  @Test def copyToArrayStartLen(): Unit = {

    def check(string: String, start: Int, len: Int, array: Array[Char], copied: Int, after: Array[Char]): Unit = {
      assertEquals(copied, string.copyToArray(array, start, len))
      assert(array.sameElements(after))
    }

    check(string = "", start = 0, len = 0, array = Array(), copied = 0, after = Array())
    check(string = "", start = 1, len = 0, array = Array(), copied = 0, after = Array())
    check(string = "", start = 1, len = 1, array = Array(), copied = 0, after = Array())

    check(string = "abcd", start = -1, len = 0, array = Array('x', 'y'), copied = 0, after = Array('x', 'y'))
    check(string = "abcd", start = 0, len = 0, array = Array('x', 'y'), copied = 0, after = Array('x', 'y'))
    check(string = "abcd", start = 0, len = 1, array = Array('x', 'y'), copied = 1, after = Array('a', 'y'))
    check(string = "abcd", start = 1, len = 1, array = Array('x', 'y'), copied = 1, after = Array('x', 'a'))
    check(string = "abcd", start = 0, len = 2, array = Array('x', 'y'), copied = 2, after = Array('a', 'b'))
    check(string = "abcd", start = 0, len = 20, array = Array('x', 'y'), copied = 2, after = Array('a', 'b'))
    check(string = "abcd", start = 7, len = 20, array = Array('x', 'y'), copied = 0, after = Array('x', 'y'))
  }

  @Test def *(): Unit = {
    assertEquals("aaa", "a" * 3)
    assertEquals("", "a" * 0)
    assertEquals("", "a" * -1)
  }

  @Test def withFilterAndThenMap(): Unit = {
    assertEquals("hello".withFilter(_ != 'e').map(_.toUpper), "HLLO")
  }

  @Test def collect(): Unit = {
    assertEquals("de", "abcdef".collect { case c @ ('b' | 'c') => (c+2).toChar })
    assertEquals(Seq('d'.toInt, 'e'.toInt), "abcdef".collect { case c @ ('b' | 'c') => (c+2).toInt })
  }

  @Test def init(): Unit = {
    assertEquals("ab", "abc".init)
    assertEquals("a", "ab".init)
    assertEquals("", "a".init)
    assertThrows[UnsupportedOperationException]("".init)
  }

  @Test def tail(): Unit = {
    assertEquals("bc", "abc".tail)
    assertEquals("b", "ab".tail)
    assertEquals("", "a".tail)
    assertThrows[UnsupportedOperationException]("".tail)
  }

  // Test that String.split(Char) matches String.split(String) in all cases
  @Test def splitByChar: Unit = {
    val separators =
      val lowSurrogate = 0xDF62.toChar
      val highSurrogate = 0xD852.toChar
      Array('D', lowSurrogate, highSurrogate)

    // add in charaters that won't be used for splitting
    val alphabet =
      separators.flatMap(ch => Seq(ch, (ch + 1).toChar))

    def allStringsOfLength(n: Int): Iterator[String] =
      if n == 0 then Iterator("")
      else
        for
          before <- allStringsOfLength(n - 1)
          ch <- alphabet
        yield before + ch

    for
      len <- 0 to 5
      s <- allStringsOfLength(len)
      separator <- separators
    do
      assertSameElements(s.split(separator), s.split(s"$separator"))
  }

  @Test def splitByCharExamples: Unit = {
    assertSameElements("a,b,c".split(','), Array("a", "b", "c"))

    assertSameElements("abc".split(','), Array("abc"))

    assertSameElements(",a,b".split(','), Array("", "a", "b"))
    assertSameElements("a,,b".split(','), Array("a", "", "b"))

    // Edge cases:
    assertSameElements("a,b,,,".split(','), Array("a", "b"))
    assertSameElements("".split(','), Array(""))
    assertSameElements(",,,".split(','), Array.empty[String])
  }

  @Test def splitBySurrogateCharExamples: Unit = {
    val high = '\uD83D' // High surrogate
    val low  = '\uDE00' // Low surrogate
    val pair = "\uD83D\uDE00" // Valid surrogate pair

    // --- HIGH SURROGATE SPLITS ---
    
    // Basic isolated high surrogate split
    assertSameElements(s"a${high}b".split(high), Array("a", "b"))

    // Should skip the high surrogate because it belongs to a valid pair
    assertSameElements(s"a${pair}b".split(high), Array(s"a${pair}b"))

    // --- LOW SURROGATE SPLITS ---
    
    // Basic isolated low surrogate split
    assertSameElements(s"a${low}b".split(low), Array("a", "b"))

    // Should skip the low surrogate because it belongs to a valid pair
    assertSameElements(s"a${pair}b".split(low), Array(s"a${pair}b"))

    // Edge case: Over-trimming recovery
    // The while loop initially trims the trailing `low` char. 
    // However, because it's preceded by a `high` char, it forms a valid pair and MUST be restored.
    assertSameElements(s"a${pair}".split(low), Array(s"a${pair}"))
  }
}