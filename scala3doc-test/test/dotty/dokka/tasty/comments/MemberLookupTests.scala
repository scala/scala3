package dotty.dokka.tasty.comments

import scala.tasty.Reflection

import org.junit.{Test, Rule}
import org.junit.Assert.{assertSame, assertTrue}
import dotty.dokka.BuildInfo

class LookupTestCases[R <: Reflection](val r: R) {

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
      val Some((lookedUp, _)) = MemberLookup.lookupOpt(parseQuery(query), None)
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

  given r.type = r

  def parseQuery(query: String): Query = {
    val Right(parsed) = QueryParser(query).tryReadQuery()
    parsed
  }

  case class Sym(symbol: r.Symbol) {
    def fld(name: String) =
      def hackResolveModule(s: r.Symbol): r.Symbol =
        if s.flags.is(r.Flags.Object) then s.moduleClass else s
      Sym(hackResolveModule(symbol.field(name)))
    def fun(name: String) =
      val List(sym) = symbol.method(name)
      Sym(sym)
    def tpe(name: String) = Sym(symbol.typeMember(name))
  }

  def cls(fqn: String) = Sym(r.Symbol.classSymbol(fqn))
}

class MemberLookupTests {

  @Test
  def test(): Unit = {
    import scala.tasty.inspector.TastyInspector
    class Inspector extends TastyInspector:
      var alreadyRan: Boolean = false

      override def processCompilationUnit(using ctx: quoted.QuoteContext)(root: ctx.reflect.Tree): Unit =
        if !alreadyRan then
          this.test()(using ctx.reflect)
          alreadyRan = true

      def test()(using r: Reflection): Unit = {
        import dotty.dokka.tasty.comments.MemberLookup

        val cases = LookupTestCases[r.type](r)

        cases.testAll()
      }

    Inspector().inspect("", listOurClasses())
  }

  def listOurClasses(): List[String] = {
    import java.io.File
    import scala.collection.mutable.ListBuffer

    val classRoot = new File(BuildInfo.testOuputDir)

    def go(bld: ListBuffer[String])(file: File): Unit =
      file.listFiles.foreach { f =>
        if f.isFile() then
          if f.toString.endsWith(".tasty") then
            bld.append(f.toString
              .stripPrefix(classRoot.toString + "/")
              .stripSuffix(".tasty")
              .replaceAll("/", ".")
            )
        else go(bld)(f)
      }

    if classRoot.isDirectory then
      val bld = new ListBuffer[String]
      go(bld)(classRoot)
      bld.result
    else
      sys.error(s"Class root could not be found: $classRoot")
  }
}
