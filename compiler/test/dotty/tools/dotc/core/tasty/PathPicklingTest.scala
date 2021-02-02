package dotty.tools.dotc.core.tasty

import java.io.{File => JFile, ByteArrayOutputStream, IOException}
import java.nio.file.{Files, NoSuchFileException, Path, Paths}

import scala.sys.process._

import org.junit.Test
import org.junit.Assert.{assertEquals, assertTrue, assertFalse, fail}

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.ast.tpd.TreeOps
import dotty.tools.dotc.{Driver, Main}
import dotty.tools.dotc.decompiler
import dotty.tools.dotc.core.Comments.CommentsContext
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Decorators.{toTermName, toTypeName}
import dotty.tools.dotc.core.Mode
import dotty.tools.dotc.core.Names.Name
import dotty.tools.dotc.interfaces.Diagnostic.ERROR
import dotty.tools.dotc.reporting.TestReporter
import dotty.tools.io.{Directory, File, Path}

import dotty.tools.vulpix.TestConfiguration

class PathPicklingTest {

  @Test def test(): Unit = {
    val out = JFile("out/testPathPickling")
    val cwd = JFile("").getAbsolutePath()
    delete(out)
    out.mkdir()

    locally {
      val ignorantProcessLogger = ProcessLogger(_ => ())
      val options = TestConfiguration.defaultOptions
        .and("-d", s"$out/out.jar")
        .and("-sourceroot", "tests/pos")
        .and(s"$cwd/tests/pos/i10430/lib.scala", s"$cwd/tests/pos/i10430/app.scala")
      val reporter = TestReporter.reporter(System.out, logLevel = ERROR)
      val rep = Main.process(options.all, reporter)
      assertFalse("Compilation failed.", rep.hasErrors)
    }

    val decompiled =
      val outstream = new ByteArrayOutputStream()
      val options = TestConfiguration.defaultOptions
        .and("-print-tasty")
        .and("-color:never")
        .and(s"$out/out.jar")
      val reporter = TestReporter.reporter(System.out, logLevel = ERROR)
      val rep = Console.withOut(outstream) {
        decompiler.Main.process(options.all, reporter)
      }
      assertFalse("Decompilation failed.", rep.hasErrors)
      new String(outstream.toByteArray(), "UTF-8")

    assertTrue(decompiled.contains(": i10430/lib.scala"))
    assertTrue(decompiled.contains(": i10430/app.scala"))
    assertTrue(decompiled.contains("[i10430/lib.scala]"))
    assertTrue(decompiled.contains("[i10430/app.scala]"))

    assertFalse(decompiled.contains(": i10430\\lib.scala"))
    assertFalse(decompiled.contains(": i10430\\app.scala"))
    assertFalse(decompiled.contains("[i10430\\lib.scala]"))
    assertFalse(decompiled.contains("[i10430\\app.scala]"))
  }

  private def delete(file: JFile): Unit = {
    if (file.isDirectory) file.listFiles.foreach(delete)
    try Files.delete(file.toPath)
    catch {
      case _: NoSuchFileException => // already deleted, everything's fine
    }
  }
}
