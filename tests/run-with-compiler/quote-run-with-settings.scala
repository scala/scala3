
import java.nio.file.{Files, Paths}

import dotty.tools.dotc.quoted.Toolbox._
import dotty.tools.dotc.quoted.ToolboxSettings

import scala.quoted._

object Test {
  def main(args: Array[String]): Unit = {
    implicit val toolbox: scala.quoted.Toolbox = dotty.tools.dotc.quoted.Toolbox.make
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
      implicit val settings = ToolboxSettings.make(outDir = Some(outDir.toString))
      implicit val toolbox2: scala.quoted.Toolbox = dotty.tools.dotc.quoted.Toolbox.make
      println(expr.run)
      assert(Files.exists(classFile))
    }
  }
}
