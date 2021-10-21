package dotty.tools.benchmarks

import dotty.tools.dotc._
import core.Contexts.Context
import dotty.tools.FatalError
import reporting._

import org.openjdk.jmh.results.RunResult
import org.openjdk.jmh.runner.Runner
import org.openjdk.jmh.runner.options.{OptionsBuilder, CommandLineOptions}
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.results.format._
import java.util.concurrent.TimeUnit

import java.io.{File, FileOutputStream, BufferedWriter, FileWriter}
import scala.collection.JavaConverters._
import scala.io.Source
import scala.util.Using

import dotty.tools.io.AbstractFile

object Bench {
  val COMPILE_OPTS_FILE = "compile.txt"

  def printUsage() =
    println("Usage (from SBT): scala3-bench/jmh:run <JMH arguments> -- <scalac arguments>")
    println("Display JMH help: scala3-bench/jmh:run -h")
    println("Our default JMH options: -wi 30 -i 20 -f 3 -tu ms -bm AverageTime -jvmArgs \"-Xms2G -Xmx2G\"")

  def main(args: Array[String]): Unit = {
    if (args.isEmpty) {
      println("Missing arguments.")
      printUsage()
      return
    }

    val (jmhArgs, _scalacArgs) = args.span(_ != "--")
    val scalacArgs = _scalacArgs.drop(1)

    storeCompileOptions(scalacArgs)

    val jmhCliOps = new CommandLineOptions(jmhArgs:_*)
    val jmhOps = new OptionsBuilder().parent(jmhCliOps)

    // set our own default options
    if !jmhCliOps.shouldFailOnError().hasValue() then jmhOps.shouldFailOnError(true)
    if !jmhCliOps.getWarmupIterations().hasValue() then jmhOps.warmupIterations(30)
    if !jmhCliOps.getMeasurementIterations().hasValue() then jmhOps.measurementIterations(20)
    if !jmhCliOps.getForkCount().hasValue() then jmhOps.forks(1)
    if jmhCliOps.getBenchModes().isEmpty() then jmhOps.mode(Mode.AverageTime)
    if !jmhCliOps.getTimeUnit().hasValue() then jmhOps.timeUnit(TimeUnit.MILLISECONDS)
    if !jmhCliOps.getJvmArgs().hasValue() then jmhOps.jvmArgs("-Xms2G", "-Xmx2G")

    val runner = new Runner(jmhOps.build())

    if jmhCliOps.shouldHelp() then
      printUsage()
      println("Following is the JMH options documentation.")
      println("-------------------------------------------")
      return jmhCliOps.showHelp()
    if jmhCliOps.shouldList() then return runner.list()
    if jmhCliOps.shouldListWithParams() then return runner.listWithParams(jmhCliOps)
    if jmhCliOps.shouldListProfilers() then return jmhCliOps.listProfilers()
    if jmhCliOps.shouldListResultFormats() then return jmhCliOps.listResultFormats()

    runner.run() // actually run the benchmarks

    removeCompileOptions
  }

  def removeCompileOptions: Unit = new File(COMPILE_OPTS_FILE).delete()

  def storeCompileOptions(args: Array[String]): Unit = {
    import File.{ separator => sep }

    val standard_libs = System.getProperty("BENCH_CLASS_PATH")
    val compiler_libs = System.getProperty("BENCH_COMPILER_CLASS_PATH")

    val libs = if (args.contains("-with-compiler")) compiler_libs else standard_libs
    var argsNorm = args.filter(_ != "-with-compiler").map { arg =>
      if ((arg.endsWith(".scala") || arg.endsWith(".java")) && !(new File(arg)).isAbsolute) ".." + sep + arg
      else arg
    }

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
