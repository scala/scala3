package dotty.tools.dotc.util

import scala.language.unsafeNulls

import org.junit.Assert._
import org.junit.Test

class DiffUtilTests {

  def testExpected(found: String, expected: String, foundColoring: String, expectedColoring: String): Unit = {
    def humanAscii(str: String): String =
      str
        .replace(Console.RESET, ">")
        .replace(Console.BOLD,  "")
        .replace(Console.RED,   "<R|")
        .replace(Console.GREEN, "<G|")
        // merging two aligning colors
        .replace("><R|", "")
        .replace("><G|", "")

    val diff = DiffUtil.mkColoredTypeDiff(found, expected)
    val fnd = humanAscii(diff._1)
    val exp = humanAscii(diff._2)

    if (fnd != foundColoring)    fail(s"expected(found):\n$foundColoring but was: \n$fnd")
    if (exp != expectedColoring) fail(s"expected(expected): \n$expectedColoring but was: \n$exp")
  }

  @Test
  def simpleString(): Unit = {
    testExpected("Foo", "Bar", "<R|Foo>", "<G|Bar>")
    testExpected("Bar", "Foo", "<R|Bar>", "<G|Foo>")
  }

  @Test
  def tuple(): Unit = {
    testExpected("(Foo, Bar)", "(Bar, Foo)", "(<R|Foo>, <R|Bar>)", "(<G|Bar>, <G|Foo>)")
    testExpected("(Int, Bar, Float)", "Bar", "<R|(Int, >Bar<R|, Float)>", "Bar")
  }

  @Test
  def tupleSeq(): Unit = {
    testExpected("(Foo, Seq[Bar])", "Seq[Bar]", "<R|(Foo, >Seq[Bar]<R|)>", "Seq[Bar]")
    testExpected("Seq[Bar]", "(Foo, Seq[Bar])", "Seq[Bar]", "<G|(Foo, >Seq[Bar]<G|)>")
  }

  @Test
  def seqTuple(): Unit = {
    testExpected("Seq[(Foo, Bar)]", "Seq[Bar]", "Seq[<R|(Foo, >Bar<R|)>]", "Seq[Bar]")
    testExpected("Seq[Bar]", "Seq[(Foo, Bar)]", "Seq[Bar]", "Seq[<G|(Foo, >Bar<G|)>]")
  }

  @Test
  def seqSeq(): Unit = {
    testExpected("Seq[Seq[Seq[Foo]]]", "Seq[List[Seq[(Bar, Foo)]]]", "Seq[<R|Seq>[Seq[Foo]]]", "Seq[<G|List>[Seq[<G|(Bar, >Foo<G|)>]]]")
    testExpected("Seq[List[Seq[(Bar, Foo)]]]", "Seq[Seq[Seq[Foo]]]", "Seq[<R|List>[Seq[<R|(Bar, >Foo<R|)>]]]", "Seq[<G|Seq>[Seq[Foo]]]")
  }

}
