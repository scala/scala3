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
         |case class Direct(i: Int):
         |  override def toString: String = "external-direct(" + i + ")"
         |
         |case class FromTrait(i: Int) extends TraitToString
         |
         |case class FromBase(i: Int) extends BaseToString
         |
         |case class Plain(i: Int)
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

      run("externalproduct.Plain(4)")(using afterBase)
      val plain = storedOutput()
      assertTrue(plain, plain.contains("val res3: externalproduct.Plain = Plain(4)"))
