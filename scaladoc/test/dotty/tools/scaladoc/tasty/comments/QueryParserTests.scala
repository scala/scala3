package dotty.tools.scaladoc
package tasty.comments

import org.junit.{Test, Rule}
import org.junit.Assert.{assertSame, assertTrue, assertEquals}

class QueryParserTests {
  @Test def test() = {
    import Query._

    def l2q(shorthand: ((String | Qual), Char)*)(last: String): QuerySegment = {
      if shorthand.isEmpty then Query.Id(last) else {
        val head = shorthand.head
        val tail = shorthand.tail
        head match {
          case ((id: String), ch) => Query.QualifiedId(Query.Qual.Id(id), ch, l2q(tail : _*)(last))
          case ((qual: Qual), ch) => Query.QualifiedId(qual, ch, l2q(tail : _*)(last))
        }
      }
    }

    extension [A <: String | Qual](self: A) def dot = (self, '.')
    extension [A <: String | Qual](self: A) def hash = (self, '#')

    testSuccess("#abc", StrictMemberId("abc"))
    testSuccess("a.b.c#d", l2q("a".dot, "b".dot, "c".hash)("d"))

    testSuccess("`a.b c#d`", Id("a.b c#d"))
    testSuccess("#`a.b c#d`", StrictMemberId("a.b c#d"))

    testSuccess("a.`b.c#d e`.g", l2q("a".dot, "b.c#d e".dot)("g"))
    testSuccess("a.`b.c#d e`#g", l2q("a".dot, "b.c#d e".hash)("g"))

    testSuccess("this.foo", l2q(Qual.This.dot)("foo"))
    testSuccess("package.foo", l2q(Qual.Package.dot)("foo"))

    testSuccess("`this`.foo", l2q("this".dot)("foo"))
    testSuccess("`package`.foo", l2q("package".dot)("foo"))

    testSuccess("#foo(ignoredOverloadDefinition*", StrictMemberId("foo"))
    testSuccess("#bar[ignoredOverloadDefinition*", StrictMemberId("bar"))

    testSuccess("\\#abc", Id("#abc"))
    testSuccess("a\\.b", Id("a.b"))
    testSuccess("a\\#b", Id("a#b"))
    testSuccess("ab\\ ", Id("ab "))

    testSuccess("#foo\\(ignoredOverloadDefinition*", StrictMemberId("foo(ignoredOverloadDefinition*"))
    testSuccess("#bar\\[ignoredOverloadDefinition*", StrictMemberId("bar[ignoredOverloadDefinition*"))

    testFailAt("#", 1)
    testFailAt("#`", 2)
    testFailAt("``", 2)
    testFailAt("`abc", 4)

    testFailAt("ab..cd", 3)
    testFailAt("ab.#cd", 3)
    testFailAt("ab#.cd", 3)

    testFailAt("\\`", 1)
    testFailAt("ab\\`", 3)
  }

  private def parse(input: String) = QueryParser(input).tryReadQuery()

  private def testSuccess(input: String, expected: Query) = {
    val Right(got) = parse(input): @unchecked
    assertEquals(expected, got)
  }

  private def testFailAt(input: String, char: Int) = {
    val Left(err) = parse(input): @unchecked
    assertEquals(s"expected to fail at $char : $input", char, err.at)
  }
}
