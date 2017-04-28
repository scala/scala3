package dotty.tools
package dotc
package parsing

import scala.reflect.io._
import scala.io.Codec
import util._
import Tokens._, Scanners._
import org.junit.Test

class ScannerTest extends DottyTest {

  val blackList = List(
      "/scaladoc/scala/tools/nsc/doc/html/page/Index.scala",
      "/scaladoc/scala/tools/nsc/doc/html/page/Template.scala"
    )

  def scan(name: String): Unit = scan(new PlainFile(name))

  def scan(file: PlainFile): Unit = {
    //println("***** scanning " + file)
    val source = new SourceFile(file, Codec.UTF8)
    val scanner = new Scanner(source)
    var i = 0
    while (scanner.token != EOF) {
//    print("[" + scanner.token.show +"]")
      scanner.nextToken
//      i += 1
//      if (i % 10 == 0) println()
    }
  }

  def scanDir(path: String): Unit = scanDir(Directory(path))

  def scanDir(dir: Directory): Unit = {
    if (blackList exists (dir.jfile.toString endsWith _))
      println(s"blacklisted package: ${dir.jfile.getAbsolutePath}")
    else
      for (f <- dir.files)
        if (f.name.endsWith(".scala"))
          if (blackList exists (f.jfile.toString endsWith _))
            println(s"blacklisted file: ${f.jfile.getAbsolutePath}")
          else
            scan(new PlainFile(f))
    for (d <- dir.dirs)
      scanDir(d.path)
  }

  @Test
  def scanList() = {
    println(System.getProperty("user.dir"))
    scan("./src/dotty/tools/dotc/core/Symbols.scala")
    scan("./src/dotty/tools/dotc/core/Symbols.scala")
  }

  @Test
  def scanDotty() = {
    scanDir("src")
  }

  @Test
  def scanScala() = {
    scanDir("../scala-library/src")
  }
}
