package dotty.tools.benchmarks

import org.openjdk.jmh.results.RunResult
import org.openjdk.jmh.runner.Runner
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.results.format._
import org.openjdk.jmh.runner.options._
import java.util.concurrent.TimeUnit

import scala.io.Source
import scala.util.Using

object Bench {
  def main(args: Array[String]): Unit = {
    if (args.contains("--help")) {
      printUsage()
      return
    }

    val (intArgs, args1) = args.span(x => try { x.toInt; true } catch { case _: Throwable => false } )

    def getIntArg(i: Int, default: Int): Int =
      if (intArgs.length > i) intArgs(i).toInt else default

    val warmup = getIntArg(0, 20)
    val iterations = getIntArg(1, 20)
    val forks = getIntArg(2, 1)

    if (args1.isEmpty) {
      println("Error: no benchmark was specified.")
      printUsage()
      return
    }

    var builder = new OptionsBuilder()
      .shouldFailOnError(true)
      .jvmArgs("-Xms2G", "-Xmx2G")
      .mode(Mode.AverageTime)
      .timeUnit(TimeUnit.NANOSECONDS)
      .warmupIterations(warmup)
      .warmupTime(TimeValue.milliseconds(750))
      .measurementIterations(iterations)
      .measurementTime(TimeValue.milliseconds(500))
      .forks(forks)
      .include(args1(0))

    if (args1.length > 1 && args1(1) != "--") {
      for ((param, values) <- paramsFromFile("inputs/" ++ args1(1)))
        builder = builder.param(param, values: _*)
    }

    if (args1.length > 2) {
      builder = builder.resultFormat(ResultFormatType.CSV).result(args1(2))
    }

    val runner = new Runner(builder.build)
    runner.run
  }

  def paramsFromFile(file: String): Array[(String, Array[String])] = {
    Using(Source.fromFile(file))(_.getLines.toArray).get.map { l =>
      val Array(param, values) = l split ':'
      (param, values split ',')
    }
  }

  def printUsage(): Unit = {
    println()
    println("Usage:")
    println()
    println("dotty-bench-run/jmh:run [<warmup>] [<iterations>] [<forks>] <regexp> [<input>|--] [<output>]")
    println()
    println("warmup: warmup iterations. defaults to 20.")
    println("iterations: benchmark iterations. defaults to 20.")
    println("forks: number of forks. defaults to 1.")
    println("regexp: regular expression that selects which benchmarks to run.")
    println("input: input vector file. each line should have format \'<paramName>: <comma-separated-values>\'")
    println("output: output file for the results of benchmarks.")
    println()
  }
}
