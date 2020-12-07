package dotty.tools
package dotc
package parsing

import dotty.tools.io._
import scala.io.Codec
import util._
import Tokens._, Scanners._
import org.junit.Test

class DottyScannerTest extends ScannerTest {

  val blackList = List(
      "/scaladoc/scala/tools/nsc/doc/html/page/Index.scala",
      "/scaladoc/scala/tools/nsc/doc/html/page/Template.scala"
    )

  def scanDir(path: String): Unit = scanDir(Directory(path))

  def scanDir(dir: Directory): Unit = {
    if (blackList exists (dir.jpath.toString endsWith _))
      println(s"blacklisted package: ${dir.toAbsolute.jpath}")
    else
      for (f <- dir.files)
        if (f.name.endsWith(".scala"))
          if (blackList exists (f.jpath.toString endsWith _))
            println(s"blacklisted file: ${f.toAbsolute.jpath}")
          else
            scan(new PlainFile(f))
    for (d <- dir.dirs)
      scanDir(d.path)
  }

  @Test
  def scanList() = {
    println(System.getProperty("user.dir"))
    scan("compiler/src/dotty/tools/dotc/core/Symbols.scala")
    scan("compiler/src/dotty/tools/dotc/core/Symbols.scala")
  }

  @Test
  def scanDotty() = {
    scanDir("compiler/src")
  }
}