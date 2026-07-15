package dotty.tools
package repl

import scala.language.unsafeNulls

import org.junit.Test
import org.junit.Assert.assertEquals

class ReplayTests extends ReplTest, SessionFileHelpers {

  @Test def replaysDefinitions =
    initially {
      run("val x = 21")
    } andThen {
      run("def double(n: Int) = n * 2")
    } andThen {
      run(":replay")
    } andThen {
      storedOutput()
      run("double(x)")
      assertEquals("val res0: Int = 42", storedOutput().trim)
    }

  @Test def replaysRedefinitionInOrder =
    initially {
      run("val x = 1")
    } andThen {
      run("val x = 2")
    } andThen {
      run(":replay")
    } andThen {
      storedOutput()
      run("x")
      assertEquals("val res0: Int = 2", storedOutput().trim)
    }

  @Test def replaysMutuallyRecursiveDefsAsOneUnit =
    initially {
      run("""|def isEven(n: Int): Boolean = if n == 0 then true else isOdd(n - 1)
             |def isOdd(n: Int): Boolean = if n == 0 then false else isEven(n - 1)""".stripMargin)
    } andThen {
      run(":replay")
    } andThen {
      storedOutput()
      run("isEven(10)")
      assertEquals("val res0: Boolean = true", storedOutput().trim)
    }

  @Test def replaysRecordedCommand =
    val jar = emptyJar()
    initially {
      run(s":jar $jar")
    } andThen {
      storedOutput()
      run(":replay")
      assertEquals(
        s"""|Replaying REPL session.
            |Added '$jar' to classpath.""".stripMargin,
        storedOutput().trim
      )
    }

  @Test def replayIsNotRecorded =
    val target = tempFile()
    initially {
      run("val a = 1")
    } andThen {
      run(":replay")
    } andThen {
      run(s":save $target")
      assertEquals(
        s"""|$header
            |$sep
            |val a = 1""".stripMargin,
        contentOf(target)
      )
    }

  @Test def announcesSettings = initially {
    run(":replay -source:future")
    assertEquals(
      """|Replaying REPL session with the following settings:
         |  -source:future""".stripMargin,
      storedOutput().trim
    )
  }

  @Test def replayLeavesOptionHistoryIntact =
    val target = tempFile()
    initially {
      run("val x = 1")
    } andThen {
      run(":settings -source:future")
    } andThen {
      run("val y = 2")
    } andThen {
      run(":replay -deprecation")
    } andThen {
      run(s":save $target")
      assertEquals(
        s"""|$header
            |$sep
            |val x = 1
            |$sep
            |:settings -source:future
            |$sep
            |val y = 2""".stripMargin,
        contentOf(target)
      )
    }
}
