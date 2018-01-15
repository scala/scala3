
import java.nio.file.{Files, Paths}

import dotty.tools.dotc.quoted.Runners._

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

    val outDir = Paths.get("../out/out-quoted-1")
    val classFile = outDir.resolve("Quoted.class")

    Files.deleteIfExists(classFile)

    val settings = RunSettings(optimise = true, outDir = Some(outDir.toString))

    println(run(expr, settings))
    assert(Files.exists(classFile))
  }
}
