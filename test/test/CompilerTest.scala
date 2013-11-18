package test

import scala.reflect.io._
import org.junit.Test
import scala.collection.mutable.ListBuffer
import dotty.tools.dotc.Main
import dotty.tools.dotc.reporting.Reporter

class CompilerTest extends DottyTest {

  def defaultOptions: List[String] = Nil

  def compileArgs(args: Array[String], xerrors: Int = 0): Unit = {
    val nerrors = Main.process(args ++ defaultOptions).count(Reporter.ERROR.level)
    assert(nerrors == xerrors, s"Wrong # of errors. Expected: $xerrors, found: $nerrors")
  }

  def compileLine(cmdLine: String, xerrors: Int = 0): Unit = compileArgs(cmdLine.split("\n"), xerrors)

  def compileFile(prefix: String, fileName: String, args: List[String] = Nil, xerrors: Int = 0): Unit =
    compileArgs((s"$prefix$fileName.scala" :: args).toArray, xerrors)

}