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
          case ((id: String), ch) => Query.QualifiedId(Query.Qual.Id(id), ch, l2q(tail*)(last))
          case ((qual: Qual), ch) => Query.QualifiedId(qual, ch, l2q(tail*)(last))
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

    // Test overloaded method signature parsing
    testSuccess("processBuffer[A](b:scala.collection.mutable.Buffer[A])*", Id("processBuffer[A](b:scala.collection.mutable.Buffer[A])*"))
    testSuccess("processMap[K,V](m:scala.collection.mutable.Map[K,V])*", Id("processMap[K,V](m:scala.collection.mutable.Map[K,V])*"))
    testSuccess("processSet[A](s:scala.collection.mutable.Set[A])*", Id("processSet[A](s:scala.collection.mutable.Set[A])*"))
    testSuccess("processSeq[A](s:scala.collection.mutable.Seq[A])*", Id("processSeq[A](s:scala.collection.mutable.Seq[A])*"))
    testSuccess("processIterator[A](it:scala.collection.Iterator[A])*", Id("processIterator[A](it:scala.collection.Iterator[A])*"))
    testSuccess("processIterable[A](it:scala.collection.mutable.Iterable[A])*", Id("processIterable[A](it:scala.collection.mutable.Iterable[A])*"))
    testSuccess("processList[A](l:scala.collection.immutable.List[A])*", Id("processList[A](l:scala.collection.immutable.List[A])*"))
    testSuccess("processProperties(p:java.util.Properties)*", Id("processProperties(p:java.util.Properties)*"))

    // Test qualified identifiers with overloaded method signatures
    testSuccess("tests.overloadedMethods.OverloadedMethods.processBuffer[A](b:scala.collection.mutable.Buffer[A])*",
      l2q("tests".dot, "overloadedMethods".dot, "OverloadedMethods".dot)("processBuffer[A](b:scala.collection.mutable.Buffer[A])*"))
    testSuccess("tests.overloadedMethods.OverloadedMethods.processMap[K,V](m:scala.collection.mutable.Map[K,V])*",
      l2q("tests".dot, "overloadedMethods".dot, "OverloadedMethods".dot)("processMap[K,V](m:scala.collection.mutable.Map[K,V])*"))
    testSuccess("tests.overloadedMethods.OverloadedMethods.processSet[A](s:scala.collection.mutable.Set[A])*",
      l2q("tests".dot, "overloadedMethods".dot, "OverloadedMethods".dot)("processSet[A](s:scala.collection.mutable.Set[A])*"))

    // Test method with multiple parameter types in signature
    testSuccess("transform[K,V](m:scala.collection.mutable.Map[K,V])*", Id("transform[K,V](m:scala.collection.mutable.Map[K,V])*"))
    testSuccess("transform[A](b:scala.collection.mutable.Buffer[A])*", Id("transform[A](b:scala.collection.mutable.Buffer[A])*"))
    testSuccess("transform(x:Int)*", Id("transform(x:Int)*"))

    // Test edge cases for overload signatures
    // Nested generics
    testSuccess("foo[A](x:Map[String,List[A]])*", Id("foo[A](x:Map[String,List[A]])*"))
    testSuccess("f[A,B](x:Either[Option[A],List[B]])*", Id("f[A,B](x:Either[Option[A],List[B]])*"))

    // Multiple parameters
    testSuccess("bar(a:Int,b:String)*", Id("bar(a:Int,b:String)*"))
    testSuccess("baz(a:Int,b:String,c:Boolean)*", Id("baz(a:Int,b:String,c:Boolean)*"))
    testSuccess("merge(a:Buffer[_],b:Set[_])*", Id("merge(a:Buffer[_],b:Set[_])*"))

    // Empty parameters
    testSuccess("noArgs()*", Id("noArgs()*"))

    // No type parameters
    testSuccess("simpleMethod(x:Int)*", Id("simpleMethod(x:Int)*"))
    testSuccess("twoParams(a:String,b:Boolean)*", Id("twoParams(a:String,b:Boolean)*"))

    // Deeply nested types
    testSuccess("complex[A](x:Map[String,Map[Int,List[A]]])*", Id("complex[A](x:Map[String,Map[Int,List[A]]])*"))

    // Custom/user-defined types
    testSuccess("process(x:tests.CustomType)*", Id("process(x:tests.CustomType)*"))
    testSuccess("handle(x:com.example.MyClass)*", Id("handle(x:com.example.MyClass)*"))

    // Qualified identifiers with complex signatures
    testSuccess("pkg.Class.method[A](x:Map[String,List[A]])*",
      l2q("pkg".dot, "Class".dot)("method[A](x:Map[String,List[A]])*"))
    testSuccess("a.b.c.method(a:Int,b:String)*",
      l2q("a".dot, "b".dot, "c".dot)("method(a:Int,b:String)*"))

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
