package dotty.tools.dotc.core.tasty

import scala.jdk.CollectionConverters.*
import org.junit.Test
import org.junit.Assert.{assertFalse, assertTrue}
import dotty.tools.dotc.Main
import dotty.tools.dotc.classpath.ClassPath
import dotty.tools.dotc.interfaces.Diagnostic.ERROR
import dotty.tools.dotc.reporting.TestReporter
import dotty.tools.nio.*
import dotty.tools.vulpix.TestConfiguration

import scala.io.Codec

/** Runs the tools that read TASTy back on freshly compiled code. */
class TastyToolsTest:

  /** Compile `source` with `options` and return the output directory. */
  private def compile(name: String, source: String, options: String*): FileContainer =
    val dirPath = s"out/testTastyTools/$name"
    FileContainer.getOnDisk(dirPath).foreach(_.deleteRecursively())
    val dir = FileContainer.getOrCreateOnDisk(dirPath)
    val classes = dir.getOrCreateContainer("classes")
    val src = dir.getOrCreateFile(s"$name.scala")
    src.writeText(source, Codec.UTF8)
    val flags = TestConfiguration.defaultOptions.and(options*).and("-d", classes.path, src.path)
    val reporter = Main.process(flags.all, TestReporter.reporter(System.out, logLevel = ERROR))
    assertFalse(s"Compilation of $name failed.", reporter.hasErrors)
    classes

  private def tastyFiles(classes: FileContainer): List[File] =
    classes.recursiveEntries.collect{ case f: File if f.extension.isTasty => f }.toList

  private def classpath(classes: FileContainer): String =
    s"${TestConfiguration.basicClasspath}${ClassPath.pathSeparator}${classes.path}"

  private def fromTasty(classes: FileContainer): Unit =
    val out = classes.getOrCreateContainer("from-tasty")
    val args = Array("-from-tasty", "-color:never", "-d", out.path, "-classpath", classpath(classes))
      ++ tastyFiles(classes).map(_.path)
    val reporter = Main.process(args, TestReporter.reporter(System.out, logLevel = ERROR))
    assertFalse("-from-tasty failed.", reporter.hasErrors)

  /** Decompile the TASTy in `classes` and return the decompiled source. */
  private def decompile(classes: FileContainer): String =
    val out = classes.getOrCreateContainer("decompiled")
    val args = Array("-color:never", "-d", out.path, "-classpath", classpath(classes))
      ++ tastyFiles(classes).map(_.path)
    val reporter = dotty.tools.dotc.decompiler.Main.process(args, TestReporter.reporter(System.out, logLevel = ERROR))
    assertFalse("Decompilation failed.", reporter.hasErrors)
    out.getOrCreateFile("decompiled.scala").readText(Codec.UTF8)

  private def printTasty(classes: FileContainer): String =
    tastyFiles(classes).map(f => TastyPrinter.showContents(f.readBytes(), noColor = true, isBestEffortTasty = false)).mkString

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

  @Test def decompileSeqWildcard(): Unit =
    val decompiled = decompile(compile("decompileSeqWildcard",
      """def f(l: List[Int]) = l match
        |  case List(1, xs*) => xs
        |  case _ => Nil
        |""".stripMargin))
    assertTrue(decompiled, decompiled.contains("_*"))

  @Test def decompileCaptureAnnotations(): Unit =
    val decompiled = decompile(compile("decompileCaptureAnnotations",
      """import language.experimental.captureChecking
        |class C extends caps.SharedCapability
        |class Ref(val c: C):
        |  val get: () ->{c} Int = () => 1
        |def f(c: C) =
        |  val r = Ref(c)
        |  r.get
        |""".stripMargin))
    assertTrue(decompiled, decompiled.contains("retains"))
