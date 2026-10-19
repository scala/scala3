package dotty.tools.dotc.core.tasty

import java.io.{File => JFile}
import java.nio.file.{Files, Path}

import scala.jdk.CollectionConverters.*

import org.junit.Test
import org.junit.Assert.{assertFalse, assertTrue}

import dotty.tools.dotc.Main
import dotty.tools.dotc.interfaces.Diagnostic.ERROR
import dotty.tools.dotc.reporting.TestReporter
import dotty.tools.io.Directory
import dotty.tools.vulpix.TestConfiguration

/** Runs the tools that read TASTy back on freshly compiled code. */
class TastyToolsTest:

  /** Compile `source` with `options` and return the output directory. */
  private def compile(name: String, source: String, options: String*): Path =
    val dir = Path.of(s"out/testTastyTools/$name")
    Directory(dir).deleteRecursively()
    val classes = Files.createDirectories(dir.resolve("classes"))
    val src = dir.resolve(s"$name.scala")
    Files.writeString(src, source)
    val flags = TestConfiguration.defaultOptions.and(options*).and("-d", classes.toString, src.toString)
    val reporter = Main.process(flags.all, TestReporter.reporter(System.out, logLevel = ERROR))
    assertFalse(s"Compilation of $name failed.", reporter.hasErrors)
    classes

  private def tastyFiles(classes: Path): List[Path] =
    Files.walk(classes).iterator.asScala.filter(_.toString.endsWith(".tasty")).toList

  private def classpath(classes: Path): String =
    s"${TestConfiguration.basicClasspath}${JFile.pathSeparator}$classes"

  private def fromTasty(classes: Path): Unit =
    val out = Files.createDirectories(classes.resolveSibling("from-tasty"))
    val args = Array("-from-tasty", "-color:never", "-d", out.toString, "-classpath", classpath(classes))
      ++ tastyFiles(classes).map(_.toString)
    val reporter = Main.process(args, TestReporter.reporter(System.out, logLevel = ERROR))
    assertFalse("-from-tasty failed.", reporter.hasErrors)

  private def printTasty(classes: Path): String =
    tastyFiles(classes).map(f => TastyPrinter.showContents(Files.readAllBytes(f), noColor = true, isBestEffortTasty = false)).mkString

  @Test def printSelectOuter(): Unit =
    // the inline body refers to the outer `this` through the self alias, pickled as SELECTouter
    val classes = compile("printSelectOuter",
      """trait O[T]:
        |  outer =>
        |  def c: Int
        |  private trait R extends O[T]:
        |    inline def c = outer.c
        |""".stripMargin)
    assertTrue(printTasty(classes).contains("SELECTouter"))

  @Test def printModifierTags(): Unit =
    val tracked = compile("printTracked", "class A(tracked val x: Int)\n", "-source", "future", "-language:experimental.modularity")
    assertTrue(printTasty(tracked).contains("TRACKED"))
    val into = compile("printInto", "import Conversion.into\ninto trait T\n")
    assertTrue(printTasty(into).contains("INTO"))
    val subMatch = compile("printSubMatch",
      """import scala.language.experimental.subCases
        |def f(x: Option[Option[Int]]) = x match
        |  case Some(y) if y match case Some(z) => z
        |  case _ => 0
        |""".stripMargin)
    assertTrue(printTasty(subMatch).contains("SUBMATCH"))

  @Test def fromTastyQuotePattern(): Unit =
    // the only quotes are patterns, which also need staging when read back from TASTy
    fromTasty(compile("fromTastyQuotePattern",
      """import scala.quoted.*
        |def isTuple[A](t: Type[A])(using Quotes): Boolean = t match
        |  case '[h *: t] => true
        |  case _ => false
        |""".stripMargin))
