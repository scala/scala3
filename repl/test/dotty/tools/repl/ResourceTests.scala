package dotty.tools
package repl

import org.junit.Assert.{assertEquals, assertFalse, assertTrue}
import org.junit.Test

class ResourceTests extends ReplTest, SessionFileHelpers:

  @Test def `resource command adds a directory to the classpath`: Unit =
    val dir = resourceDir("greeting.txt", "hello")
    initially {
      run(s":resource $dir")
    } andThen {
      assertEquals(s"Added '$dir' to classpath.", storedOutput().trim)
      run("""val greeting = scala.io.Source.fromResource("greeting.txt").mkString""")
      val output = storedOutput()
      assertTrue(output, output.contains("""val greeting: String = "hello""""))
    }

  @Test def `resource command adds a single file to the classpath`: Unit =
    val file = resourceDir("conf.json", "{}").resolve("conf.json")
    initially {
      run(s":resource $file")
    } andThen {
      assertEquals(s"Added '$file' to classpath.", storedOutput().trim)
      run("""val conf = scala.io.Source.fromResource("conf.json").mkString""")
      val output = storedOutput()
      assertTrue(output, output.contains("""val conf: String = "{}""""))
    }

  private def inOneSession(inputs: String*): Unit =
    initially:
      runBody:
        inputs.foldLeft(summon[State]): (current, input) =>
          interpretSubmission(ParseResult.complete(input)(using current))(using current)

  @Test def `resource added mid-session is readable by later code`: Unit =
    val dir = resourceDir("greeting.txt", "hello")
    inOneSession(
      s":resource $dir",
      """val greeting = scala.io.Source.fromResource("greeting.txt").mkString"""
    )
    val output = storedOutput()
    assertTrue(output, output.contains("""val greeting: String = "hello""""))

  @Test def `adding a resource preserves already initialized definitions`: Unit =
    val dir = resourceDir("greeting.txt", "hello")
    initially {
      run("val id = java.util.UUID.randomUUID.toString")
    } andThen {
      val id = storedOutput().split('=').last.trim
      run(s":resource $dir")
      storedOutput()
      run("id")
      val output = storedOutput()
      assertTrue(s"$id was re-initialized: $output", output.contains(id))
    }

  @Test def `resource command reports a path that does not exist`: Unit =
    val missing = resourceDir("present.txt", "x").resolve("absent.txt")
    initially:
      run(s":resource $missing")
      assertEquals(s"Cannot add '$missing' to classpath, it does not exist.", storedOutput().trim)

  @Test def `resource command survives a path the platform rejects`: Unit =
    initially {
      run(":resource \u0000nope")
    } andThen {
      val output = storedOutput()
      assertTrue(output, output.contains("Failed to load"))
      run("val alive = 1")
      val afterwards = storedOutput()
      assertTrue(afterwards, afterwards.contains("val alive: Int = 1"))
    }

  @Test def `resource added after a reset is readable by later code`: Unit =
    val dir = resourceDir("greeting.txt", "hello")
    inOneSession(
      ":reset",
      s":resource $dir",
      """val greeting = scala.io.Source.fromResource("greeting.txt").mkString"""
    )
    val output = storedOutput()
    assertTrue(output, output.contains("""val greeting: String = "hello""""))

  @Test def `reset drops resources added before it`: Unit =
    val dir = resourceDir("greeting.txt", "hello")
    inOneSession(
      s":resource $dir",
      ":reset",
      """val gone = scala.util.Try(scala.io.Source.fromResource("greeting.txt")).isFailure"""
    )
    val output = storedOutput()
    assertTrue(output, output.contains("val gone: Boolean = true"))

  @Test def `resource added after a reset in one submission is readable by trailing code`: Unit =
    val dir = resourceDir("greeting.txt", "hello")
    inOneSession(
      s""":reset
         |:resource $dir
         |val greeting = scala.io.Source.fromResource("greeting.txt").mkString""".stripMargin
    )
    val output = storedOutput()
    assertTrue(output, output.contains("""val greeting: String = "hello""""))
