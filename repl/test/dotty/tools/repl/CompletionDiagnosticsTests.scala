package dotty.tools
package repl

import java.nio.file.Files

import org.junit.{AfterClass, Test}
import org.junit.Assert.{assertEquals, assertTrue}

object CompletionDiagnosticsTests:
  private val classpath = Files.createTempDirectory("repl-completion-diagnostics")
  private def invalidClassfile(tag: Byte) = Array[Byte](
    0xca.toByte,
    0xfe.toByte,
    0xba.toByte,
    0xbe.toByte,
    0,
    0,
    0,
    52, // Java 8 classfile header
    0,
    2, // constant_pool_count (one entry)
    tag // invalid tag for entry #1
  )
  private val brokenBytes = invalidClassfile(0)
  private val brokenClassfile =
    Files.write(Files.createDirectories(classpath.resolve("broken")).resolve("Broken.class"), brokenBytes)
  private val incidentalClassfile =
    Files.write(Files.createDirectories(classpath.resolve("scala")).resolve("CompletionPoison.class"), brokenBytes)

  def options: Array[String] =
    ReplTest.createOptions(classpath.toString).filterNot(_ == "-Ydebug")

  @AfterClass def tearDownFixture(): Unit =
    Files.delete(brokenClassfile)
    Files.delete(incidentalClassfile)
    Files.delete(brokenClassfile.getParent)
    Files.delete(incidentalClassfile.getParent)
    Files.delete(classpath)

class CompletionDiagnosticsTests extends ReplTest(options = CompletionDiagnosticsTests.options):

  private def assertSingleLoadingError(output: String, name: String): Unit =
    assertTrue(output, output.contains(s"error while loading $name"))
    assertTrue(output, output.contains("is broken"))
    assertEquals(1, output.linesIterator.count(_.contains(s"error while loading $name")))

  @Test def `i18614 loading errors are reported for every explicit completion`: Unit =
    initially:
      tabComplete("import broken.Broken.")
      assertSingleLoadingError(storedOutput(), "Broken")

      tabComplete("import broken.Broken.")
      assertSingleLoadingError(storedOutput(), "Broken")

  @Test def `ordinary completion errors remain hidden`: Unit =
    initially:
      tabComplete("List.doesNotExist")
      assertEquals("", storedOutput())

  @Test def `incidental loading errors remain available to an explicit completion`: Unit =
    initially {
      val state = run("object O")
      storedOutput()
      Files.write(CompletionDiagnosticsTests.incidentalClassfile, CompletionDiagnosticsTests.invalidClassfile(99))
      state
    } andThen {
      try
        tabComplete("O.")
        assertEquals("", storedOutput())

        // Changing the file again proves that the explicit query replays the failure found by `O.`
        // instead of attempting the physical load for the first time.
        Files.write(CompletionDiagnosticsTests.incidentalClassfile, Array[Byte](0, 0, 0, 0))
        tabComplete("import scala.CompletionPoison.")
        val output = storedOutput()
        assertSingleLoadingError(output, "CompletionPoison")
        assertTrue(output, output.contains("bad constant pool tag 99"))
      finally
        Files.write(CompletionDiagnosticsTests.incidentalClassfile, CompletionDiagnosticsTests.brokenBytes)
    }
