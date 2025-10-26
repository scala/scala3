package dotty.tools
package repl

import scala.language.unsafeNulls

import java.io.{ByteArrayOutputStream, PrintStream}
import java.nio.charset.StandardCharsets

import vulpix.TestConfiguration
import org.junit.Test
import org.junit.Assert._

/** Tests for the programmatic REPL API (ReplMain) */
class ReplMainTest:

  private val defaultOptions = Array("-classpath", TestConfiguration.withCompilerClasspath)

  private def captureOutput(body: PrintStream => Unit): String =
    val out = new ByteArrayOutputStream()
    val ps = new PrintStream(out, true, StandardCharsets.UTF_8.name)
    body(ps)
    dotty.shaded.fansi.Str(out.toString(StandardCharsets.UTF_8.name)).plainText

  @Test def basicBinding(): Unit =
    val output = captureOutput { out =>
      val replMain = new ReplMain(
        settings = defaultOptions,
        out = out,
        testCode = "test"
      )

      replMain.run("test" -> 42)
    }

    assertTrue(output.contains("val res0: Int = 42"))

  @Test def multipleBindings(): Unit =
    val output = captureOutput { out =>
      val replMain = new ReplMain(
        settings = defaultOptions,
        out = out,
        testCode = "x\ny\nz"
      )

      replMain.run(
        "x" -> 1,
        "y" -> "hello",
        "z" -> true
      )
    }

    assertTrue(output.contains("val res0: Int = 1"))
    assertTrue(output.contains("val res1: String = \"hello\""))
    assertTrue(output.contains("val res2: Boolean = true"))

  @Test def bindingTypes(): Unit =
    val output = captureOutput { out =>
      val replMain = new ReplMain(
        settings = defaultOptions ++ Array("-repl-quit-after-init"),
        out = out,
        testCode = "list\nmap"
      )

      replMain.run(
        "list" -> List(1, 2, 3),
        "map" -> Map(1 -> "hello")
      )
    }

    assertTrue(output.contains("val res0: List[Int] = List(1, 2, 3)"))
    assertTrue(output.contains("val res1: Map[Int, String] = Map(1 -> \"hello\")"))

end ReplMainTest
