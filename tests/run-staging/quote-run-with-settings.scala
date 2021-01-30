
import java.nio.file.{Files, Paths}

import scala.quoted.*
import scala.quoted.staging.*

object Test {
  def main(args: Array[String]): Unit = {
    given Compiler = Compiler.make(getClass.getClassLoader)
    def expr(using Quotes) = '{
      val a = 3
      println("foo")
      2 + a
    }
    println(withQuotes(expr.show))
    println(run(expr))
    println()

    val outDir = Paths.get("out/out-quoted-1")
    val classFile = outDir.resolve("Generated$Code$From$Quoted.class")

    Files.deleteIfExists(classFile)

    {
      implicit val settings = Compiler.Settings.make(outDir = Some(outDir.toString))
      implicit val toolbox2: scala.quoted.staging.Compiler = scala.quoted.staging.Compiler.make(getClass.getClassLoader)
      println(run(expr))
      assert(Files.exists(classFile))
    }
  }
}
