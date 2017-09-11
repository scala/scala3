package dotty.tools.benchmarks

import dotty.tools.dotc._
import core.Contexts.Context

import org.openjdk.jmh.results.RunResult
import org.openjdk.jmh.runner.Runner
import org.openjdk.jmh.runner.options.OptionsBuilder
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.results.format._
import java.util.concurrent.TimeUnit

import java.io.{File, FileOutputStream, BufferedWriter, FileWriter}
import scala.collection.JavaConversions._
import scala.io.Source

object Bench {
  val COMPILE_OPTS_FILE = "compile.txt"

  def main(args: Array[String]): Unit = {
    if (args.isEmpty) {
      println("Missing <args>")
      return
    }
    val (intArgs, args1) = args.span(x => try { x.toInt; true } catch { case _: Throwable => false } )

    val warmup = if (intArgs.length > 0) intArgs(0).toInt else 30
    val iteration warmup = if (intArgs.length > 1) intArgs(1).toInt else 20

    val args2 = args1.map { arg =>
      if ((arg.endsWith(".scala") || arg.endsWith(".java")) && arg.head != '/') "../" + arg
      else arg
    }
    storeCompileOptions(args2)

    val libs = System.getProperty("BENCH_CLASS_PATH")

    val opts = new OptionsBuilder()
               .jvmArgsPrepend("-Xbootclasspath/a:" + libs + ":")
               .mode(Mode.AverageTime)
               .timeUnit(TimeUnit.MILLISECONDS)
               .forks(1)
               .warmupIterations(warmup)
               .measurementIterations(iterations)
               .build

    val runner = new Runner(opts) // full access to all JMH features, you can also provide a custom output Format here
    runner.run() // actually run the benchmarks

    removeCompileOptions
  }

  def removeCompileOptions: Unit = new File(COMPILE_OPTS_FILE).delete()

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
    val res = process(state.opts)
    if (res.hasErrors) throw new Exception("compilation failed")
  }
}
