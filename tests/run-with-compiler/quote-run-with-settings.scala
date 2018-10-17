
import java.nio.file.{Files, Paths}

import scala.quoted._

object Test {
  def main(args: Array[String]): Unit = {
    val tb = Toolbox.make(getClass.getClassLoader)

    def expr: Staged[Int] = '{
      val a = 3
      println("foo")
      2 + a
    }
    println(tb.show(expr))
    println(tb.run(expr))
    println()

    val outDir = Paths.get("out/out-quoted-1")
    val classFile = outDir.resolve("Generated$Code$From$Quoted.class")

    Files.deleteIfExists(classFile)

    {
      implicit val settings = Toolbox.Settings.make(outDir = Some(outDir.toString))
      implicit val toolbox2: scala.quoted.Toolbox = scala.quoted.Toolbox.make(getClass.getClassLoader)
      println(toolbox2.run(expr))
      assert(Files.exists(classFile))
    }
  }
}
