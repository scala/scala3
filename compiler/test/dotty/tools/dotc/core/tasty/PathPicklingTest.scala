package dotty.tools.dotc.core.tasty

import scala.sys.process.*
import org.junit.Test
import org.junit.Assert.{assertFalse, assertTrue}
import dotty.tools.dotc.Main
import dotty.tools.dotc.interfaces.Diagnostic.ERROR
import dotty.tools.dotc.reporting.TestReporter
import dotty.tools.nio.*
import dotty.tools.vulpix.TestConfiguration

class PathPicklingTest {

  @Test def test(): Unit = {
    FileContainer.getOnDisk("out/testPathPickling").foreach(_.deleteRecursively())
    val outPath = "out/testPathPickling/out.jar"

    locally {
      val ignorantProcessLogger = ProcessLogger(_ => ())
      val options = TestConfiguration.defaultOptions
        .and("-d", outPath)
        .and("-sourceroot", "tests/pos")
        .and(s"tests/pos/i10430/lib.scala", s"tests/pos/i10430/app.scala")
      val reporter = TestReporter.reporter(System.out, logLevel = ERROR)
      val rep = Main.process(options.all, reporter)
      assertFalse("Compilation failed.", rep.hasErrors)
    }

    val printedTasty =
      val sb = new StringBuffer
      val jar = FileContainer.getOnDisk(outPath).get
      try
        for file <- jar.entries.collect { case f: File if f.extension.isTasty => f } do
          sb.append(TastyPrinter.showContents(file.readBytes(), noColor = true, isBestEffortTasty = false))
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
