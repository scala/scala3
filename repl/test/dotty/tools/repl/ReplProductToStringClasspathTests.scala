package dotty.tools
package repl

import scala.language.unsafeNulls

import dotc.Driver
import dotc.interfaces.Diagnostic.ERROR
import dotc.reporting.TestReporter
import vulpix.{TestConfiguration, TestFlags}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.util.Comparator

import org.junit.Assert.{assertFalse, assertTrue}
import org.junit.{AfterClass, Test}

object ReplProductToStringClasspathTests:
  private var tempDir: Path = null

  private lazy val fixtureJar: Path =
    tempDir = Files.createTempDirectory("repl-product-tostring-classpath")
    createFixtureJar(tempDir)

  def options: Array[String] =
    ReplTest.createOptions(fixtureJar.toAbsolutePath.toString)

  private def createFixtureJar(root: Path): Path =
    val srcDir = Files.createDirectories(root.resolve("src"))
    val source = srcDir.resolve("ExternalProducts.scala")
    Files.writeString(
      source,
      """|package externalproduct
         |
         |trait TraitToString:
         |  override def toString: String = "external-trait"
         |
         |abstract class BaseToString:
         |  override def toString: String = "external-base"
         |
         |abstract class BaseSymbolOnlyToString extends Product:
         |  override def toString: String = scala.runtime.ScalaRunTime._toString(this)
         |
         |case class Direct(i: Int):
         |  override def toString: String = "external-direct(" + i + ")"
         |
         |case class FromTrait(i: Int) extends TraitToString
         |
         |case class FromBase(i: Int) extends BaseToString
         |
         |case class Plain(i: Int, s: String)
         |
         |case class ProductPair(first: Product, second: Product)
         |
         |case class SymbolOnlyToString(i: Int, s: String):
         |  override def toString: String = scala.runtime.ScalaRunTime._toString(this)
         |
         |object Factory:
         |  def localDirect: Product =
         |    case class LocalDirect(i: Int):
         |      override def toString: String = "external-local-direct(" + i + ")"
         |    LocalDirect(5)
         |
         |  def localInherited: Product =
         |    case class LocalInherited(i: Int) extends TraitToString
         |    LocalInherited(6)
         |
         |  def localInheritedSymbolOnly: Product =
         |    case class LocalInheritedSymbolOnly(i: Int, s: String) extends BaseSymbolOnlyToString
         |    LocalInheritedSymbolOnly(15, "declaring")
         |
         |  def localPlain: Product =
         |    case class LocalPlain(i: Int, s: String)
         |    LocalPlain(7, "local")
         |
         |  def localDefaultShaped: Product =
         |    case class LocalDefaultShaped(i: Int, s: String):
         |      override def toString: String = "LocalDefaultShaped(" + i + "," + s + ")"
         |    LocalDefaultShaped(13, "same")
         |
         |  def anonymousDirect: Product =
         |    new Product:
         |      def canEqual(that: Any): Boolean = that.isInstanceOf[Product]
         |      def productArity: Int = 1
         |      def productElement(n: Int): Any =
         |        if n == 0 then 8 else throw new IndexOutOfBoundsException(n.toString)
         |      override def productPrefix: String = "ExternalAnonymousDirect"
         |      override def toString: String = "external-anonymous-direct"
         |
         |  def anonymousInherited: Product =
         |    new Product with TraitToString:
         |      def canEqual(that: Any): Boolean = that.isInstanceOf[Product]
         |      def productArity: Int = 1
         |      def productElement(n: Int): Any =
         |        if n == 0 then 9 else throw new IndexOutOfBoundsException(n.toString)
         |      override def productPrefix: String = "ExternalAnonymousInherited"
         |
         |  def anonymousInheritedSymbolOnly: Product =
         |    new BaseSymbolOnlyToString:
         |      def canEqual(that: Any): Boolean = that.isInstanceOf[Product]
         |      def productArity: Int = 2
         |      def productElement(n: Int): Any =
         |        if n == 0 then 16
         |        else if n == 1 then "anonymous"
         |        else throw new IndexOutOfBoundsException(n.toString)
         |      override def productElementName(n: Int): String =
         |        if n == 0 then "i"
         |        else if n == 1 then "s"
         |        else throw new IndexOutOfBoundsException(n.toString)
         |      override def productPrefix: String = "ExternalAnonymousSymbolOnly"
         |
         |  def anonymousInheritedSymbolOnlyAgain: Product =
         |    new BaseSymbolOnlyToString:
         |      def canEqual(that: Any): Boolean = that.isInstanceOf[Product]
         |      def productArity: Int = 2
         |      def productElement(n: Int): Any =
         |        if n == 0 then 17
         |        else if n == 1 then "again"
         |        else throw new IndexOutOfBoundsException(n.toString)
         |      override def productElementName(n: Int): String =
         |        if n == 0 then "i"
         |        else if n == 1 then "s"
         |        else throw new IndexOutOfBoundsException(n.toString)
         |      override def productPrefix: String = "ExternalAnonymousSymbolOnlyAgain"
         |
         |  def anonymousInheritedSymbolOnlyPair: ProductPair =
         |    ProductPair(anonymousInheritedSymbolOnly, anonymousInheritedSymbolOnlyAgain)
         |
         |  def anonymousPlain: Product =
         |    new Product:
         |      def canEqual(that: Any): Boolean = that.isInstanceOf[Product]
         |      def productArity: Int = 1
         |      def productElement(n: Int): Any =
         |        if n == 0 then 10 else throw new IndexOutOfBoundsException(n.toString)
         |      override def productPrefix: String = "ExternalAnonymousPlain"
         |
         |class Outer:
         |  case class InnerDirect(i: Int):
         |    override def toString: String = "external-inner-direct(" + i + ")"
         |
         |  case class InnerPlain(i: Int, s: String)
         |""".stripMargin,
      StandardCharsets.UTF_8
    )

    val jar = root.resolve("external-products.jar")
    val flags =
      TestFlags(TestConfiguration.basicClasspath, TestConfiguration.noCheckOptions)
        .and("-d", jar.toString)
    val reporter = TestReporter.reporter(System.out, logLevel = ERROR)
    new Driver().process(flags.all :+ source.toString, reporter)
    assertFalse(s"compilation of $source failed", reporter.hasErrors)
    assertTrue(s"compiler did not create $jar", Files.isRegularFile(jar))
    jar

  @AfterClass def tearDownFixture: Unit =
    if tempDir != null then
      Files.walk(tempDir)
        .sorted(Comparator.reverseOrder)
        .forEach(Files.delete)
      tempDir = null

class ReplProductToStringClasspathTests extends ReplTest(options = ReplProductToStringClasspathTests.options):

  private def assertRenderedAs(output: String, expected: String, structuralPrefix: String): Unit =
    assertTrue(output, output.contains(expected))
    assertFalse(output, output.contains(structuralPrefix))

  @Test def `product toString from startup classpath`: Unit =
    initially:
      val afterDirect = run("externalproduct.Direct(1)")
      assertRenderedAs(storedOutput(), "val res0: externalproduct.Direct = external-direct(1)", "Direct(1)")

      val afterTrait = run("externalproduct.FromTrait(2)")(using afterDirect)
      assertRenderedAs(storedOutput(), "val res1: externalproduct.FromTrait = external-trait", "FromTrait(2)")

      val afterBase = run("externalproduct.FromBase(3)")(using afterTrait)
      assertRenderedAs(storedOutput(), "val res2: externalproduct.FromBase = external-base", "FromBase(3)")

      val afterPlain = run("""externalproduct.Plain(4, "plain")""")(using afterBase)
      val plain = storedOutput()
      assertTrue(plain, plain.contains("""val res3: externalproduct.Plain = Plain(i = 4, s = "plain")"""))
      assertFalse(plain, plain.contains("Plain(4,plain)"))

      val afterLocalDirect = run("externalproduct.Factory.localDirect")(using afterPlain)
      assertRenderedAs(storedOutput(), "val res4: Product = external-local-direct(5)", "LocalDirect(5)")

      val afterLocalInherited = run("externalproduct.Factory.localInherited")(using afterLocalDirect)
      assertRenderedAs(storedOutput(), "val res5: Product = external-trait", "LocalInherited(6)")

      // The local product class is unresolved, and bytecode fallback would reject
      // the inherited method body. This passes via the declaring-class symbol.
      val afterLocalInheritedSymbolOnly = run("externalproduct.Factory.localInheritedSymbolOnly")(using afterLocalInherited)
      assertRenderedAs(
        storedOutput(),
        "val res6: Product = LocalInheritedSymbolOnly(15,declaring)",
        """LocalInheritedSymbolOnly(i = 15, s = "declaring")"""
      )

      val afterLocalPlain = run("externalproduct.Factory.localPlain")(using afterLocalInheritedSymbolOnly)
      val localPlain = storedOutput()
      assertTrue(localPlain, localPlain.contains("""val res7: Product = LocalPlain(i = 7, s = "local")"""))
      assertFalse(localPlain, localPlain.contains("LocalPlain(7,local)"))

      val afterLocalDefaultShaped = run("externalproduct.Factory.localDefaultShaped")(using afterLocalPlain)
      assertRenderedAs(
        storedOutput(),
        "val res8: Product = LocalDefaultShaped(13,same)",
        """LocalDefaultShaped(i = 13, s = "same")"""
      )

      val afterOuter = run("val externalOuter = new externalproduct.Outer")(using afterLocalDefaultShaped)
      storedOutput()

      val afterInnerDirect = run("externalOuter.InnerDirect(11)")(using afterOuter)
      assertRenderedAs(storedOutput(), "val res9: externalOuter.InnerDirect = external-inner-direct(11)", "InnerDirect(11)")

      val afterInnerPlain = run("""externalOuter.InnerPlain(12, "inner")""")(using afterInnerDirect)
      val innerPlain = storedOutput()
      assertTrue(innerPlain, innerPlain.contains("""val res10: externalOuter.InnerPlain = InnerPlain(i = 12, s = "inner")"""))
      assertFalse(innerPlain, innerPlain.contains("InnerPlain(12,inner)"))

      val afterAnonymousDirect = run("externalproduct.Factory.anonymousDirect")(using afterInnerPlain)
      assertRenderedAs(storedOutput(), "val res11: Product = external-anonymous-direct", "ExternalAnonymousDirect(8)")

      val afterAnonymousInherited = run("externalproduct.Factory.anonymousInherited")(using afterAnonymousDirect)
      assertRenderedAs(storedOutput(), "val res12: Product = external-trait", "ExternalAnonymousInherited(9)")

      // Same declaring-class symbol path, but for an anonymous product class.
      val afterAnonymousInheritedSymbolOnly = run("externalproduct.Factory.anonymousInheritedSymbolOnly")(using afterAnonymousInherited)
      assertRenderedAs(
        storedOutput(),
        "val res13: Product = ExternalAnonymousSymbolOnly(16,anonymous)",
        """ExternalAnonymousSymbolOnly(i = 16, s = "anonymous")"""
      )

      val afterAnonymousInheritedSymbolOnlyAgain =
        run("externalproduct.Factory.anonymousInheritedSymbolOnlyAgain")(using afterAnonymousInheritedSymbolOnly)
      assertRenderedAs(
        storedOutput(),
        "val res14: Product = ExternalAnonymousSymbolOnlyAgain(17,again)",
        """ExternalAnonymousSymbolOnlyAgain(i = 17, s = "again")"""
      )

      val afterAnonymousInheritedSymbolOnlyPair =
        run("externalproduct.Factory.anonymousInheritedSymbolOnlyPair")(using afterAnonymousInheritedSymbolOnlyAgain)
      val anonymousInheritedSymbolOnlyPair = storedOutput()
      assertTrue(anonymousInheritedSymbolOnlyPair, anonymousInheritedSymbolOnlyPair.contains("val res15: externalproduct.ProductPair ="))
      assertTrue(anonymousInheritedSymbolOnlyPair, anonymousInheritedSymbolOnlyPair.contains("ExternalAnonymousSymbolOnly(16,anonymous)"))
      assertTrue(anonymousInheritedSymbolOnlyPair, anonymousInheritedSymbolOnlyPair.contains("ExternalAnonymousSymbolOnlyAgain(17,again)"))
      assertFalse(
        anonymousInheritedSymbolOnlyPair,
        anonymousInheritedSymbolOnlyPair.contains("""ExternalAnonymousSymbolOnly(i = 16, s = "anonymous")""")
      )
      assertFalse(
        anonymousInheritedSymbolOnlyPair,
        anonymousInheritedSymbolOnlyPair.contains("""ExternalAnonymousSymbolOnlyAgain(i = 17, s = "again")""")
      )

      val afterAnonymousPlain = run("externalproduct.Factory.anonymousPlain")(using afterAnonymousInheritedSymbolOnlyPair)
      val anonymousPlain = storedOutput()
      assertTrue(anonymousPlain, anonymousPlain.contains("val res16: Product = ExternalAnonymousPlain(10)"))

      // This body has the same bytecode shape as synthesized case-class toString.
      // It only renders via toString if the compiler-symbol path wins.
      run("""externalproduct.SymbolOnlyToString(14, "symbol")""")(using afterAnonymousPlain)
      assertRenderedAs(
        storedOutput(),
        "val res17: externalproduct.SymbolOnlyToString = SymbolOnlyToString(14,symbol)",
        """SymbolOnlyToString(i = 14, s = "symbol")"""
      )
