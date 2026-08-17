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
    // `-Ydebug` prints exceptions from the deliberately corrupt classfiles.
    ReplTest.createOptions(classpath.toString).filterNot(_ == "-Ydebug")

  @AfterClass def tearDownFixture(): Unit =
    Files.delete(brokenClassfile)
    Files.delete(incidentalClassfile)
    Files.delete(brokenClassfile.getParent)
    Files.delete(incidentalClassfile.getParent)
    Files.delete(classpath)

class CompletionDiagnosticsTests extends ReplTest(options = CompletionDiagnosticsTests.options):

  private def assertSingleLoadingError(output: String, name: String): Unit =
    val header = s"error while loading $name"
    assertTrue(output, output.contains(header))
    assertEquals(output, 1, output.linesIterator.count(_.contains(header)))

  @Test def `explicit completion reports a loading error on every request`: Unit =
    initially:
      tabComplete("import broken.Broken.")
      assertSingleLoadingError(storedOutput(), "Broken")

      tabComplete("import broken.Broken.")
      assertSingleLoadingError(storedOutput(), "Broken")

  @Test def `missing member completion remains silent`: Unit =
    initially:
      tabComplete("List.doesNotExist")
      assertEquals("", storedOutput())

  @Test def `loading error found by completion is replayed by a later submission`: Unit =
    initially {
      val state = run("object O")
      storedOutput()
      state
    } andThen {
      try
        Files.write(CompletionDiagnosticsTests.incidentalClassfile, CompletionDiagnosticsTests.invalidClassfile(99))

        // Extension completion for `O.` enumerates the default `scala.*` import,
        // which forces `CompletionPoison` while looking for candidates.
        tabComplete("O.")
        assertEquals("", storedOutput())

        // A second load would now fail on the header, not on constant-pool tag 99.
        Files.write(CompletionDiagnosticsTests.incidentalClassfile, Array[Byte](0, 0, 0, 0))
        run("val poison: scala.CompletionPoison = ???")
        val output = storedOutput()
        assertSingleLoadingError(output, "CompletionPoison")
        assertTrue(output, output.contains("bad constant pool tag 99"))
      finally
        Files.write(CompletionDiagnosticsTests.incidentalClassfile, CompletionDiagnosticsTests.brokenBytes)
    }
