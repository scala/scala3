
import java.nio.file.{Files, Paths}

import dotty.tools.dotc.quoted.Toolbox._

import scala.quoted._

object Test {
  def main(args: Array[String]): Unit = {
    val expr = '{
      val a = 3
      println("foo")
      2 + a
    }
    println(expr.show)
    println(expr.run)
    println()

    val outDir = Paths.get("out/out-quoted-1")
    val classFile = outDir.resolve("Quoted.class")

    Files.deleteIfExists(classFile)

    {
      implicit val settings = Settings.run(optimise = true, outDir = Some(outDir.toString))
      println(expr.run)
      assert(Files.exists(classFile))
    }
  }
}
