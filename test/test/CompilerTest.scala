package test

import scala.reflect.io._
import org.junit.Test
import scala.collection.mutable.ListBuffer
import dotty.tools.dotc.{Main, Bench, Driver}
import dotty.tools.dotc.reporting.Reporter

class CompilerTest extends DottyTest {

  def compileArgs(args: Array[String], xerrors: Int = 0)(implicit defaultOptions: List[String]): Unit = {
    val allArgs = args ++ defaultOptions
    val processor = if (allArgs.exists(_.startsWith("#"))) Bench else Main
    val nerrors = processor.process(allArgs, ctx).errorCount
    assert(nerrors == xerrors, s"Wrong # of errors. Expected: $xerrors, found: $nerrors")
  }

  def compileLine(cmdLine: String, xerrors: Int = 0)(implicit defaultOptions: List[String]): Unit = compileArgs(cmdLine.split("\n"), xerrors)

  def compileFile(prefix: String, fileName: String, args: List[String] = Nil, xerrors: Int = 0)(implicit defaultOptions: List[String]): Unit =
    compileArgs((s"$prefix$fileName.scala" :: args).toArray, xerrors)

  def compileDir(path: String, args: List[String] = Nil, xerrors: Int = 0)(implicit defaultOptions: List[String]): Unit =
    compileDir(Directory(path), args, xerrors)

  def compileDir(dir: Directory, args: List[String], xerrors: Int)(implicit defaultOptions: List[String]): Unit = {
    val fileNames = dir.deepFiles.toArray.map(_.toString).filter(_ endsWith ".scala")
    compileArgs(fileNames ++ args, xerrors)
  }

  def compileFiles(path: String, args: List[String] = Nil)(implicit defaultOptions: List[String]): Unit = {
    val dir = Directory(path)
    val fileNames = dir.files.toArray.map(_.toString).filter(_ endsWith ".scala")
    for (name <- fileNames) {
      println(s"testing $name")
      compileArgs((name :: args).toArray, 0)
    }
    for (subdir <- dir.dirs) {
      println(s"testing $subdir")
      compileDir(subdir, args, 0)
    }
  }
}
object CompilerTest extends App {

//  val dotcDir = "/Users/odersky/workspace/dotty/src/dotty/"

//  new CompilerTest().compileFile(dotcDir + "tools/dotc/", "CompilationUnit")
//  new CompilerTest().compileFile(dotcDir + "tools/dotc/", "Compiler")
//  new CompilerTest().compileFile(dotcDir + "tools/dotc/", "Driver")
//  new CompilerTest().compileFile(dotcDir + "tools/dotc/", "Main")
//  new CompilerTest().compileFile(dotcDir + "tools/dotc/", "Run")

//  new CompilerTest().compileDir(dotcDir + "tools/dotc")
 // new CompilerTest().compileFile(dotcDir + "tools/dotc/", "Run")
}