package dotty.tools.dotc.core.tasty

import scala.sys.process.*
import org.junit.Test
import org.junit.Assert.{assertFalse, assertTrue}
import dotty.tools.dotc.Main
import dotty.tools.dotc.interfaces.Diagnostic.ERROR
import dotty.tools.dotc.reporting.TestReporter
import dotty.tools.io.{JarArchive, Path}
import dotty.tools.nio.*
import dotty.tools.vulpix.TestConfiguration

class PathPicklingTest {

  @Test def test(): Unit = {
    FileContainer.getOrCreateOnDisk("out/testPathPickling", "").deleteRecursively()
    val out = File.getOrCreateOnDisk("out/testPathPickling/out.jar")

    locally {
      val ignorantProcessLogger = ProcessLogger(_ => ())
      val options = TestConfiguration.defaultOptions
        .and("-d", out.path)
        .and("-sourceroot", "tests/pos")
        .and(s"tests/pos/i10430/lib.scala", s"tests/pos/i10430/app.scala")
      val reporter = TestReporter.reporter(System.out, logLevel = ERROR)
      val rep = Main.process(options.all, reporter)
      assertFalse("Compilation failed.", rep.hasErrors)
    }

    val printedTasty =
      val sb = new StringBuffer
      val jar = JarArchive.open(Path(out.path))
      try
        for file <- jar.iterator if file.name.endsWith(".tasty") do
          sb.append(TastyPrinter.showContents(file.toByteArray, noColor = true, isBestEffortTasty = false))
      finally jar.close()
      sb.toString()

    assertTrue(printedTasty.contains(": i10430/lib.scala"))
    assertTrue(printedTasty.contains("[i10430/lib.scala]"))
    assertFalse(printedTasty.contains(": i10430\\lib.scala"))
    assertFalse(printedTasty.contains("[i10430\\lib.scala]"))

    assertTrue(printedTasty.contains(": i10430/app.scala"))
    assertTrue(printedTasty.contains("[i10430/app.scala]"))
    assertFalse(printedTasty.contains(": i10430\\app.scala"))
    assertFalse(printedTasty.contains("[i10430\\app.scala]"))
  }
}
