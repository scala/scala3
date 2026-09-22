package dotty.tools
package repl

import org.junit.Assert.assertTrue
import org.junit.Test

class ClasspathStateTests extends ReplTest, SessionFileHelpers:

  private def keepsEarlierDefinitions(classpathCommand: String): Unit =
    initially {
      run("var counter = 0")
    } andThen {
      storedOutput()
      run("counter += 5")
    } andThen {
      storedOutput()
      run(classpathCommand)
    } andThen {
      storedOutput()
      run("counter")
      val output = storedOutput()
      assertTrue(output, output.contains(": Int = 5"))
    }

  @Test def `jar keeps earlier definitions`: Unit =
    keepsEarlierDefinitions(s":jar ${emptyJar()}")

  @Test def `dep keeps earlier definitions`: Unit =
    keepsEarlierDefinitions(":dep com.lihaoyi::os-lib:0.11.8")
