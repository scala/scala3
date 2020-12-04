package dotty.dokka.tasty.comments

import scala.quoted.Quotes

import org.junit.{Test, Rule}
import org.junit.Assert.{assertSame, assertTrue}
import dotty.dokka.tasty.util._

class LookupTestCases[Q <: Quotes](val q: Quotes) {

  def testAll(): Unit = {
    testOwnerlessLookup()
    testOwnedLookup()
    testStrictMemberLookup()
  }

  def testOwnerlessLookup(): Unit = {
    val cases = List[(String, Sym)](
      "tests.A" -> cls("tests.A"),
      "tests.A$" -> cls("tests.A$"),
      "tests.Methods.simple" -> cls("tests.Methods").fun("simple"),
    )

    cases.foreach { case (query, Sym(sym)) =>
      val lookupRes = MemberLookup.lookupOpt(parseQuery(query), None)
      assertTrue(s"Couldn't look up: $query", lookupRes.nonEmpty)
      val Some((lookedUp, _)) = lookupRes
      assertSame(query, sym, lookedUp)
    }
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

      cls("tests.C") -> "CC" -> cls("tests.C").fld("CC"),
      cls("tests.C") -> "CC$" -> cls("tests.C").fld("CC"),
      cls("tests.C") -> "CC!" -> cls("tests.C").tpe("CC"),

      cls("tests.A").fun("method") -> "AA" -> cls("tests.A").tpe("AA"),
      cls("tests.A").fun("method") -> "AA!" -> cls("tests.A").tpe("AA"),
      cls("tests.A").fun("method") -> "AA$" -> cls("tests.A").fld("AA"),

      cls("tests.Methods").fun("simple") -> "generic" -> cls("tests.Methods").fun("generic"),
      cls("tests.Methods").fun("simple") -> "#generic" -> cls("tests.Methods").fun("generic"),

      cls("tests.A").fun("method") -> "B" -> cls("tests.B"),
      cls("tests.A").fun("method") -> "B$" -> cls("tests.B$"),
    )

    cases.foreach { case ((Sym(owner), query), Sym(target)) =>
      val Some((lookedUp, _)) = MemberLookup.lookup(parseQuery(query), owner)
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
      Sym(hackResolveModule(symbol.field(name)))
    def fun(name: String) =
      val List(sym) = symbol.memberMethod(name)
      Sym(sym)
    def tpe(name: String) = Sym(symbol.memberType(name))
  }

  def cls(fqn: String) = Sym(q.reflect.Symbol.classSymbol(fqn))
}

class MemberLookupTests {

  @Test
  def test(): Unit = {
    import scala.tasty.inspector.TastyInspector
    class Inspector extends TastyInspector:
      var alreadyRan: Boolean = false

      override def processCompilationUnit(using ctx: quoted.Quotes)(root: ctx.reflect.Tree): Unit =
        if !alreadyRan then
          this.test()
          alreadyRan = true

      def test()(using q: Quotes): Unit = {
        import dotty.dokka.tasty.comments.MemberLookup

        val cases = LookupTestCases[q.type](q)

        cases.testAll()
      }

    Inspector().inspectTastyFiles(TestUtils.listOurClasses())
  }
}
