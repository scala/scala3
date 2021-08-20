package dotty.tools.scaladoc
package tasty.comments

import scala.quoted.Quotes

import org.junit.{Test, Rule}
import org.junit.Assert.{assertSame, assertTrue}
import dotty.tools.scaladoc.tasty.util._

class LookupTestCases[Q <: Quotes](val q: Quotes) {

  given DocContext = testDocContext()

  def testAll(): Unit = {
    testOwnerlessLookup()
    testOwnedLookup()
    testStrictMemberLookup()
  }

  def testOwnerlessLookup(): Unit = {
    val cases = List[(String, Sym)](
      "Array" -> cls("scala.Array"),
      "Option" -> cls("scala.Option"),
      "Predef$" -> cls("scala.Predef$"),
      "Predef$.identity" -> cls("scala.Predef$").fun("identity"),
      "Predef.identity" -> cls("scala.Predef$").fun("identity"),
      "Array$.from" -> cls("scala.Array$").fun("from"),
      "???" -> cls("scala.Predef$").fun("???"),
      "scala.List" -> cls("scala.package$").tpe("List"),

      "scala.List.lift" -> cls("scala.PartialFunction").fun("lift"),

      "tests.A" -> cls("tests.A"),
      "tests.A$" -> cls("tests.A$"),
      "tests.Methods.simple" -> cls("tests.Methods").fun("simple"),
      "tests.foo" -> cls("tests.package$").fld("foo"),
      "tests.bar" -> cls("tests.tests$package$").fld("bar"),

      "java.util.AbstractCollection" -> cls("java.util.AbstractCollection"),
      "java.lang.String" -> cls("java.lang.String"),
      "java.util.Formatter" -> cls("java.util.Formatter"),
      "java.io.Flushable" -> cls("java.io.Flushable"),
      "java.util.List" -> cls("java.util.List"),

      "tests.lookupInheritedMembers.pack1.A.x" ->
        cls("tests.lookupInheritedMembers.pack1.A").fun("x"),

      "tests.lookupInheritedMembers.pack2.B.x" ->
        cls("tests.lookupInheritedMembers.pack1.A").fun("x"),
    )

    cases.foreach { case (query, sym) =>
      testOwnerlessLookup(query, sym)
    }
  }

  def testOwnerlessLookup(query: String, wrappedTarget: Sym): Unit = {
    val target = wrappedTarget.symbol
    val lookupRes = MemberLookup.lookupOpt(parseQuery(query), None)
    assertTrue(s"Couldn't look up: $query", lookupRes.nonEmpty)
    val Some((lookedUp, _, _)) = lookupRes
    assertSame(query, target, lookedUp)
  }

  def testOwnedLookup(): Unit = {
    val cases = List[((Sym, String), Sym)](
      cls("tests.A") -> "tests.Methods.simple" -> cls("tests.Methods").fun("simple"),
      cls("tests.A") -> "tests#Methods#simple" -> cls("tests.Methods").fun("simple"),

      cls("tests.A") -> "method" -> cls("tests.A").fun("method"),
      cls("tests.A") -> "#method" -> cls("tests.A").fun("method"),
      cls("tests.A") -> "method*" -> cls("tests.A").fun("method"),
      cls("tests.A") -> "method[T]*" -> cls("tests.A").fun("method"),
      cls("tests.A") -> "method(str:String*" -> cls("tests.A").fun("method"),

      cls("tests.A") -> "tests.B" -> cls("tests.B"),
      cls("tests.A") -> "tests.B$" -> cls("tests.B$"),

      cls("tests.A") -> "AA" -> cls("tests.A").tpe("AA"),
      cls("tests.A") -> "#AA" -> cls("tests.A").tpe("AA"),
      cls("tests.A") -> "AA!" -> cls("tests.A").tpe("AA"),
      cls("tests.A") -> "AA$" -> cls("tests.A").fld("AA"),

      cls("tests.C") -> "CC" -> cls("tests.C").tpe("CC"),
      cls("tests.C") -> "CC$" -> cls("tests.C").fld("CC"),
      cls("tests.C") -> "CC!" -> cls("tests.C").tpe("CC"),

      cls("tests.A").fun("method") -> "AA" -> cls("tests.A").tpe("AA"),
      cls("tests.A").fun("method") -> "AA!" -> cls("tests.A").tpe("AA"),
      cls("tests.A").fun("method") -> "AA$" -> cls("tests.A").fld("AA"),

      cls("tests.Methods").fun("simple") -> "generic" -> cls("tests.Methods").fun("generic"),
      cls("tests.Methods").fun("simple") -> "#generic" -> cls("tests.Methods").fun("generic"),

      cls("tests.A").fun("method") -> "B" -> cls("tests.B"),
      cls("tests.A").fun("method") -> "B$" -> cls("tests.B$"),

      cls("tests.A") -> "B.method" -> cls("tests.B").fun("method"),
      cls("tests.A") -> "Option" -> cls("scala.Option"),

      /*sanity*/ cls("tests.A") -> "this.X" -> cls("tests.A").tpe("X"),
      /*sanity*/ cls("tests.A") -> "this.Y" -> cls("tests.A").tpe("Y"),
      cls("tests.A") -> "this.X.method" -> cls("tests.B").fun("method"),
      cls("tests.A") -> "this.Y.method" -> cls("tests.B").fun("method"),

      cls("tests.A") -> "A.foo" -> cls("tests.A$").fun("foo"),

      cls("tests.inner.B") -> "A" -> cls("tests.inner.A$"),

      cls("tests.B$") -> "foo" -> cls("tests.BModule").fun("foo"),

      cls("tests.D") -> "foo" -> cls("tests.package$").fld("foo"),
      cls("tests.D") -> "bar" -> cls("tests.tests$package$").fld("bar"),
      cls("tests.inner.A$") -> "foo" -> cls("tests.package$").fld("foo"),
      cls("tests.inner.A$") -> "bar" -> cls("tests.tests$package$").fld("bar"),
    )

    cases.foreach { case ((Sym(owner), query), Sym(target)) =>
      val Some((lookedUp, _, _)) = MemberLookup.lookup(parseQuery(query), owner)
      assertSame(s"$owner / $query", target, lookedUp)
    }
  }

  def testStrictMemberLookup(): Unit = {
    val owner = cls("tests.A").symbol
    val query = "#A"

    assertTrue("strict member lookup should not look outside", MemberLookup.lookup(parseQuery(query), owner).isEmpty)
  }

  given q.type = q

  def parseQuery(query: String): Query = {
    val Right(parsed) = QueryParser(query).tryReadQuery()
    parsed
  }

  case class Sym(symbol: q.reflect.Symbol) {
    def fld(name: String) =
      def hackResolveModule(s: q.reflect.Symbol): q.reflect.Symbol =
        if s.flags.is(q.reflect.Flags.Module) then s.moduleClass else s
      Sym(hackResolveModule(symbol.declaredField(name)))
    def fun(name: String) =
      val List(sym) = symbol.methodMember(name)
      Sym(sym)
    def tpe(name: String) = Sym(symbol.typeMember(name))
  }

  def cls(fqn: String) = Sym(q.reflect.Symbol.classSymbol(fqn))
}

class MemberLookupTests {

  @Test
  def test(): Unit = {
    import scala.tasty.inspector.OldTastyInspector
    class Inspector extends OldTastyInspector:
      var alreadyRan: Boolean = false

      override def processCompilationUnit(using ctx: quoted.Quotes)(root: ctx.reflect.Tree): Unit =
        if !alreadyRan then
          this.test()
          alreadyRan = true

      def test()(using q: Quotes): Unit = {
        import dotty.tools.scaladoc.tasty.comments.MemberLookup

        val cases = LookupTestCases[q.type](q)

        cases.testAll()
      }

    Inspector().inspectTastyFiles(TestUtils.listOurClasses())
  }
}
