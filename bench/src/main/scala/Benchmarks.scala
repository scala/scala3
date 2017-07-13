package dotty.tools.benchmarks

import dotty.tools.dotc._
import core.Contexts.Context

import org.openjdk.jmh.results.RunResult
import org.openjdk.jmh.runner.Runner
import org.openjdk.jmh.runner.options.CommandLineOptions
import org.openjdk.jmh.annotations.Benchmark
import org.openjdk.jmh.annotations.State
import org.openjdk.jmh.annotations.Scope

import org.openjdk.jmh.annotations.Setup
import org.openjdk.jmh.annotations.TearDown


import java.io.{File, FileOutputStream, BufferedWriter, FileWriter}
import scala.collection.JavaConversions._
import scala.io.Source

object Bench {
  val COMPILE_OPTS_FILE = "compile.txt"

  def main(args: Array[String]): Unit = {
    storeCompileOptions(args)

    val opts = new CommandLineOptions() // parse command line arguments, and then bend them to your will! ;-)
    val runner = new Runner(opts) // full access to all JMH features, you can also provide a custom output Format here

    /*
    val results = runner.run() // actually run the benchmarks

    val f = new FileOutputStream(new File("custom.out"))
    results.foreach { result: RunResult ⇒
      // usually you'd use these results to report into some external aggregation tool for example
      f.write(s"custom reporting result: ${result.getAggregatedResult.getPrimaryResult}".getBytes("UTF-8"))
    }
    f.close()
    */
  }

  def storeCompileOptions(args: Array[String]): Unit = {
    val file = new File(COMPILE_OPTS_FILE)
    val bw = new BufferedWriter(new FileWriter(file))
    bw.write(args.mkString("\n"))
    bw.close()
  }

  def readCompileOptions: Seq[String] =
    Source.fromFile(COMPILE_OPTS_FILE).getLines.toSeq
}

@State(Scope.Benchmark)
class CompilerOptions {
  var opts: Array[String] = null

  @Setup
  def prepare: Unit = {
    opts = Bench.readCompileOptions.to[Array]
  }
}

class Worker extends Driver {
  override def newCompiler(implicit ctx: Context): Compiler = new Compiler

  @Benchmark
  def compile(state: CompilerOptions): Unit = {
    println("options: " + state.opts.mkString(", "))
    super.main(state.opts)
  }
}
