package dotty.tools.benchmarks

import dotty.tools.dotc._
import core.Contexts.Context
import dotty.tools.FatalError
import reporting._

import org.openjdk.jmh.results.RunResult
import org.openjdk.jmh.runner.Runner
import org.openjdk.jmh.runner.options.OptionsBuilder
import org.openjdk.jmh.runner.options.TimeValue
//import org.openjdk.jmh.results.format.ResultFormatType
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.results.format._
import java.util.concurrent.TimeUnit

import java.io.{File, FileOutputStream, BufferedWriter, FileWriter}
import scala.jdk.CollectionConverters._
import scala.io.Source
import scala.util.Using

import dotty.tools.io.AbstractFile

object Bench {
  val COMPILE_OPTS_FILE = "compile.txt"
  val GENERATED_BENCHMARKS_DIR = "tests-generated"

  def main(args: Array[String]): Unit = {
    generateBenchmarks(GENERATED_BENCHMARKS_DIR)

    if (args.isEmpty) {
      println("Missing <args>")
      return
    }
    val (intArgs, args1) = args.span(x => try { x.toInt; true } catch { case _: Throwable => false } )

    val warmup = if (intArgs.length > 0) intArgs(0).toInt else 30
    val iterations = if (intArgs.length > 1) intArgs(1).toInt else 20
    val forks = if (intArgs.length > 2) intArgs(2).toInt else 1
    val measurementTime = if (intArgs.length > 3) intArgs(3).toInt else 1

    import File.{ separator => sep }

    val args2 = args1.map { arg =>
      if ((arg.endsWith(".scala") || arg.endsWith(".java")) && !(new File(arg)).isAbsolute) ".." + sep + arg
      else arg
    }
    storeCompileOptions(args2)

    val opts = new OptionsBuilder()
               .shouldFailOnError(true)
               .jvmArgs("-Xms2G", "-Xmx2G")
               .mode(Mode.AverageTime)
               .timeUnit(TimeUnit.MILLISECONDS)
               .warmupIterations(warmup)
               .warmupTime(TimeValue.seconds(measurementTime))
               .measurementIterations(iterations)
               .measurementTime(TimeValue.seconds(measurementTime))
               // To output results to bench/results.json, uncomment the 2
               // following lines and the ResultFormatType import.
               //.result("results.json")
               //.resultFormat(ResultFormatType.JSON)
               .forks(forks)
               .build

    val runner = new Runner(opts) // full access to all JMH features, you can also provide a custom output Format here
    runner.run() // actually run the benchmarks

    removeCompileOptions
  }

  def removeCompileOptions: Unit = new File(COMPILE_OPTS_FILE).delete()

  def storeCompileOptions(args: Array[String]): Unit = {
    val standard_libs = System.getProperty("BENCH_CLASS_PATH")
    val compiler_libs = System.getProperty("BENCH_COMPILER_CLASS_PATH")

    val libs = if (args.contains("-with-compiler")) compiler_libs else standard_libs
    var argsNorm = args.filter(_ != "-with-compiler")

    import File.{ pathSeparator => sep }
    var cpIndex = argsNorm.indexOf("-classpath")
    if (cpIndex == -1) cpIndex = argsNorm.indexOf("-cp")
    if (cpIndex != -1) argsNorm(cpIndex + 1) = argsNorm(cpIndex + 1) + sep + libs
    else argsNorm = argsNorm :+ "-classpath" :+ libs

    val file = new File(COMPILE_OPTS_FILE)
    val bw = new BufferedWriter(new FileWriter(file))
    bw.write(argsNorm.mkString("", "\n", "\n"))
    bw.close()
  }

  def readCompileOptions: Seq[String] =
    Using(Source.fromFile(COMPILE_OPTS_FILE))(_.getLines.toSeq).get
}

@State(Scope.Benchmark)
class CompilerOptions {
  var opts: Array[String] = null

  @Setup
  def prepare: Unit = {
    opts = Bench.readCompileOptions.toArray
  }
}

class Worker extends Driver {
  // override to avoid printing summary information
  override  def doCompile(compiler: Compiler, files: List[AbstractFile])(implicit ctx: Context): Reporter =
    if (files.nonEmpty)
      try {
        val run = compiler.newRun
        run.compile(files)
        ctx.reporter
      }
      catch {
        case ex: FatalError  =>
          report.error(ex.getMessage) // signals that we should fail compilation.
          ctx.reporter
      }
    else ctx.reporter

  @Benchmark
  def compile(state: CompilerOptions): Unit = {
    val res = process(state.opts)
    if (res.hasErrors) throw new Exception("compilation failed")
  }
}
