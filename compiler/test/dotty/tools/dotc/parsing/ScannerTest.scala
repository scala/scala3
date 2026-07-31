package dotty.tools
package dotc
package parsing

import dotty.tools.nio.*
import scala.io.Codec
import util.*
import Tokens.*, Scanners.*
import org.junit.Test

class ScannerTest extends DottyTest {

  val excluded = List(
      "/scaladoc/scala/tools/nsc/doc/html/page/Index.scala",
      "/scaladoc/scala/tools/nsc/doc/html/page/Template.scala"
    )

  def scan(name: String): Unit = scan(File.getOnDisk(name).get)

  def scan(file: File): Unit = {
    //println("***** scanning " + file)
    val source = SourceFile(file, FileContainer.getOnDisk(".").get, Codec.UTF8)
    val scanner = new Scanner(source)
    var i = 0
    while (scanner.token != EOF) {
//    print("[" + scanner.token.show +"]")
      scanner.nextToken()
//      i += 1
//      if (i % 10 == 0) println()
    }
  }

  def scanDir(path: String): Unit = scanDir(FileContainer.getOnDisk(path).get)

  def scanDir(dir: FileContainer): Unit = {
    if excluded.exists(dir.path.endsWith(_)) then
      println(s"excluded package: ${dir.path}")
    else
      for (f <- dir.entries.collect { case f: File => f })
        if (f.extension.isScala)
          if excluded.exists(f.path.endsWith(_)) then
            println(s"excluded file: ${f.path}")
          else
            scan(f)
    for (d <- dir.entries.collect { case fc: FileContainer => fc })
      scanDir(d.path)
  }

  @Test
  def scanList() = {
    scan("compiler/src/dotty/tools/dotc/core/Symbols.scala")
    scan("compiler/src/dotty/tools/dotc/core/Symbols.scala")
  }

  @Test
  def scanDotty() = {
    scanDir("src")
  }
}
