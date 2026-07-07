package dotty.tools
package repl

import scala.language.unsafeNulls

import java.io.FileOutputStream
import java.nio.file.{Path, Files}
import java.util.jar.JarOutputStream

import org.junit.Test
import org.junit.Assert.assertEquals

class SaveTests extends ReplTest {

  private def tempFile(suffix: String = ".scala"): Path =
    val file = Files.createTempFile("repl_save", suffix)
    file.toFile.deleteOnExit()
    file

  private def emptyJar(): Path =
    val jar = tempFile(".jar")
    new JarOutputStream(new FileOutputStream(jar.toFile)).close()
    jar

  private def contentOf(f: Path): String = Files.readString(f)

  private def lines() =
      storedOutput().trim.linesIterator.toList

  private val header = Save.sessionHeader
  private val sep = Save.entrySeparator

  @Test def savesSuccessfulInputs =
    val target = tempFile()
    initially {
      run("val x = 1")
    } andThen {
      run("def double(n: Int) = n * 2")
    } andThen {
      run(s":save $target")
      assertEquals(
        s"""|$header
            |$sep
            |val x = 1
            |$sep
            |def double(n: Int) = n * 2""".stripMargin,
        contentOf(target)
      )
    }

  @Test def skipsErroredInputs =
    val target = tempFile()
    initially {
      run("val x = 1")
    } andThen {
      run("val y = notDefined")
    } andThen {
      run(s":save $target")
      assertEquals(
        s"""|$header
            |$sep
            |val x = 1""".stripMargin,
        contentOf(target)
      )
    }

  @Test def recordsCommands =
    val target = tempFile()
    initially {
      run("val x = 1")
    } andThen {
      run(":type x")
    } andThen {
      run(":imports")
    } andThen {
      run(s":save $target")
      assertEquals(
        s"""|$header
            |$sep
            |val x = 1
            |$sep
            |:type x
            |$sep
            |:imports""".stripMargin,
        contentOf(target)
      )
    }

  @Test def roundTripsViaLoad =
    val target = tempFile()
    initially {
      run("val x = 21")
    } andThen {
      run("def double(n: Int) = n * 2")
    } andThen {
      run(s":save $target")
    }
    resetToInitial()
    initially {
      run(s":load $target")
    } andThen {
      storedOutput()
      run("double(x)")
      assertEquals("val res0: Int = 42", storedOutput().trim)
    }

  @Test def emptyPath = initially {
    run(":save")
    assertEquals("File name is required.", storedOutput().trim)
  }

  @Test def emptySession =
    val target = tempFile()
    initially {
      run(s":save $target")
      assertEquals("Nothing to save.", storedOutput().trim)
    }

  @Test def recordsSuccessfulJar =
    val jar = emptyJar()
    val target = tempFile()
    initially {
      run(s":jar $jar")
    } andThen {
      run(s":save $target")
      assertEquals(
        s"""|$header
            |$sep
            |:jar $jar""".stripMargin,
        contentOf(target)
      )
    }

  @Test def roundTripsCommandViaLoad =
    val jar = emptyJar()
    val target = tempFile()
    initially {
      run(s":jar $jar")
    } andThen {
      run("val x = 42")
    } andThen {
      run(s":save $target")
    }
    resetToInitial()
    initially {
      storedOutput()
      run(s":load $target")
    } andThen {
      assertEquals(
        List(
          s"Added '$jar' to classpath.",
          "val x: Int = 42"
        ),
        lines()
      )
      run("x")
      assertEquals(
        List("val res0: Int = 42"),
        lines()
      )
    }

  @Test def roundTripsRedefinitionViaLoad =
    val target = tempFile()
    initially {
      run("val x = 1")
    } andThen {
      run("val x = 2")
    } andThen {
      run(s":save $target")
    }
    resetToInitial()
    initially {
      storedOutput()
      run(s":load $target")
    } andThen {
      storedOutput()
      run("x")
      assertEquals(List("val res0: Int = 2"), lines())
    }

  @Test def roundTripsCommandInStringViaLoad =
    val target = tempFile()
    initially {
      run("val s = \"\"\"\n:help\n\"\"\"")
    } andThen {
      run(s":save $target")
    }
    resetToInitial()
    initially {
      storedOutput()
      run(s":load $target")
    } andThen {
      storedOutput()
      run("s.trim")
      assertEquals(List("val res0: String = \":help\""), lines())
    }

  @Test def roundTripsSeparatorInStringViaLoad =
    val target = tempFile()
    initially {
      run(s"""val s = "$sep"""")
    } andThen {
      run(s":save $target")
    }
    resetToInitial()
    initially {
      storedOutput()
      run(s":load $target")
    } andThen {
      storedOutput()
      run("s")
      assertEquals(List(s"""val res0: String = "$sep""""), lines())
    }

  @Test def skipsThrowingVal =
    val target = tempFile()
    initially {
      run("val x = 1")
    } andThen {
      run("""val boom = throw new RuntimeException("boom")""")
    } andThen {
      run(s":save $target")
      assertEquals(
        s"""|$header
            |$sep
            |val x = 1""".stripMargin,
        contentOf(target)
      )
    }

  @Test def recordsLoadAsCommand =
    val script = tempFile()
    Files.writeString(script, "val a = 1\nval b = 2\n")
    val target = tempFile()
    initially {
      run(s":load $script")
    } andThen {
      run(s":save $target")
      assertEquals(
        s"""|$header
            |$sep
            |:load $script""".stripMargin,
        contentOf(target)
      )
    }

  @Test def roundTripsMutuallyRecursiveDefs =
    val target = tempFile()
    initially {
      run("""|def isEven(n: Int): Boolean = if n == 0 then true else isOdd(n - 1)
             |def isOdd(n: Int): Boolean = if n == 0 then false else isEven(n - 1)""".stripMargin)
    } andThen {
      run(s":save $target")
    }
    resetToInitial()
    initially {
      storedOutput()
      run(s":load $target")
    } andThen {
      storedOutput()
      run("isEven(10)")
      assertEquals(List("val res0: Boolean = true"), lines())
    }
}
