package dotty.tools.dotc.typer

import dotty.tools.dotc.Main
import dotty.tools.dotc.interfaces.Diagnostic.ERROR
import dotty.tools.dotc.reporting.TestReporter
import dotty.tools.nio.*
import dotty.tools.vulpix.TestConfiguration
import org.junit.Test
import org.junit.Assert.{assertFalse, assertTrue}

import scala.io.Codec

/** Regression test for https://github.com/scala/scala3/issues/23043
  *
  * A compiler plugin that collects per-symbol information will naturally write
  * files into the class output directory using the symbol's full name split on
  * dots as the path, e.g.
  *
  *     val parts = symbol.fullName.split('.')
  *     val dir   = parts.init.foldLeft(outputDir)(_.subdirectoryNamed(_))
  *     dir.fileNamed(parts.last + ".myext")
  *
  * For an object `testpkg.Scope` this creates a directory `testpkg/Scope/`
  * inside the class output. The classpath scanner then treats that directory as
  * a package and on the next compilation (with the output dir on the classpath,
  * as sbt does) the compiler throws:
  *
  *     TypeError: package testpkg contains object and package with same name: Scope
  *
  * The fix skips entering a directory as a package when it conflicts with an
  * existing symbol and the directory contains no class/tasty files or
  * sub-packages.
  */
class NamerRecompileTest:

  @Test def spuriousPackageDirectoryDoesNotConflictWithObject(): Unit =
    val tmp = FileContainer.createTemporaryOnDisk("namer-recompile-test")
    try
      val srcFile = tmp.getOrCreateFile("Test.scala")
      srcFile.writeText(
        """package testpkg
          |
          |object Scope:
          |  enum MyEnum:
          |    case A
          |    case B(x: Int)
          |""".stripMargin,
        Codec.UTF8
      )

      val out = tmp.getOrCreateContainer("out")
      val outPath = out.path

      val baseOptions = TestConfiguration.defaultOptions
        .and("-d", outPath)

      // First compilation: should succeed
      val options1 = baseOptions.and(srcFile.path)
      val reporter1 = TestReporter.reporter(System.out, logLevel = ERROR)
      Main.process(options1.all, reporter1)
      assertFalse("First compilation failed.", reporter1.hasErrors)

      // Simulate a compiler plugin writing per-symbol data into the output
      // directory. A plugin that does:
      //   val parts = "testpkg.Scope".split('.')     // Array("testpkg", "Scope")
      //   val dir   = parts.init.foldLeft(out)(_.subdirectoryNamed(_))
      //   dir.fileNamed(parts.last + ".sir")
      // will create  testpkg/Scope/  which the classpath scanner sees as a package.
      val pluginDir = out.getOrCreateContainer("testpkg").getOrCreateContainer("Scope")
      val pluginFile = pluginDir.getOrCreateFile("MyEnum.sir")
      pluginFile.writeText("plugin data", Codec.UTF8)

      // Second compilation with output on classpath (as sbt does):
      // should still succeed despite the plugin-created directory
      val options2 = baseOptions.withClasspath(outPath).and(srcFile.path)
      val reporter2 = TestReporter.reporter(System.out, logLevel = ERROR)
      Main.process(options2.all, reporter2)
      assertFalse(
        "Second compilation failed with spurious package conflict (i23043).",
        reporter2.hasErrors
      )
    finally tmp.deleteRecursively()
