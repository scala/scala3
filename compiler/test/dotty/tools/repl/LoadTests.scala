package dotty.tools.repl

import java.nio.file.{ Path, Files }

import org.junit.{ Test, BeforeClass, AfterClass }
import org.junit.Assert.assertEquals

class LoadTests extends ReplTest {
  import LoadTests._

  @Test def helloworld = loadTest(
    file    = """def helloWorld = "Hello, World!"""",
    defs    = """|def helloWorld: String
                 |
                 |
                 |""".stripMargin,
    runCode = "helloWorld",
    output  = """|val res0: String = Hello, World!
                 |
                 |""".stripMargin
  )

  @Test def maindef = loadTest(
    file    = """@main def helloWorld = println("Hello, World!")""",
    defs    = """|def helloWorld: Unit
                 |
                 |
                 |""".stripMargin,
    runCode = "helloWorld",
    output  = """|Hello, World!
                 |
                 |""".stripMargin
  )

  @Test def maindefs = loadTest(
    file    = """|@main def helloWorld = println("Hello, World!")
                 |@main def helloTo(name: String) = println(s"Hello, $name!")""".stripMargin,
    defs    = """|def helloTo(name: String): Unit
                 |def helloWorld: Unit
                 |
                 |
                 |""".stripMargin,
    runCode = """helloWorld; helloTo("Scala")""",
    output  = """|Hello, World!
                 |Hello, Scala!
                 |
                 |""".stripMargin
  )

  def loadTest(file: String, defs: String, runCode: String, output: String) =
    eval(s":load ${writeFile(file)}").andThen { implicit s =>
      assertEquals(defs, storedOutput())
      run(runCode)
      assertEquals(output, storedOutput())
    }

  private def eval(code: String): State =
    fromInitialState { implicit s => run(code) }

}

object LoadTests {

  private var dir: Path = null

  @BeforeClass def setupDir: Unit =
    dir = Files.createTempDirectory("repl_load_src")

  @AfterClass def removeDir: Unit =
    Files.walk(dir).filter(!Files.isDirectory(_)).forEach(Files.delete)
    Files.delete(dir)
    dir = null

  def writeFile(contents: String): Path = {
    val file = Files.createTempFile(dir, "repl_test", ".scala")
    Files.write(file, contents.getBytes)
    file
  }

}
