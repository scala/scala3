package dotty.tools
package dotc
package repl
package test

import test._
import org.junit.Test

import dotty.tools.dotc.test.CompilerTest
import dotty.tools.io._

class ReplCompilerTest extends CompilerTest {
  val defaultOutputDir = ""
  val testsDir = "../tests/"
  val replDir = testsDir + "repl/"

  @Test def repl_all = replFiles(replDir)

  def replFile(prefix: String, fileName: String): Unit = {
    val path = s"$prefix$fileName"
    val f = new PlainFile(path)
    val repl = new TestREPL(new String(f.toCharArray))
    repl.process(Array[String]())
    repl.check()
  }

  def replFiles(path: String): Unit = {
    println(s"path: $path")
    val dir = Directory(path)
    val fileNames = dir.files.toArray.map(_.jfile.getName).filter(_ endsWith ".check")
    for (name <- fileNames) {
      log(s"testing $path$name")
      replFile(path, name)
    }
  }
}
