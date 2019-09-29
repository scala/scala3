package dotty.tools.benchmarks

import dotty.tools.dotc._
import core.Contexts.Context
import dotty.tools.FatalError
import reporting._

import org.openjdk.jmh.results.RunResult
import org.openjdk.jmh.runner.Runner
import org.openjdk.jmh.runner.options.OptionsBuilder
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.results.format._
import java.util.concurrent.TimeUnit

import scala.runtime.DynamicTuple

object Bench {
  def main(args: Array[String]): Unit = {
    val (intArgs, args1) = args.span(x => try { x.toInt; true } catch { case _: Throwable => false } )

    val warmup = if (intArgs.length > 0) intArgs(0).toInt else 30
    val iterations = if (intArgs.length > 1) intArgs(1).toInt else 20
    val forks = if (intArgs.length > 2) intArgs(2).toInt else 1

    val benchmarks = if (args1.length > 0) args1(0) else ".*"
    val outputFile = if (args1.length > 1) args1(1) else "output.csv"

    val opts = new OptionsBuilder()
               .shouldFailOnError(true)
               .jvmArgs("-Xms2G", "-Xmx2G")
               .mode(Mode.AverageTime)
               .timeUnit(TimeUnit.NANOSECONDS)
               .warmupIterations(warmup)
               .measurementIterations(iterations)
               .forks(forks)
               .include(benchmarks)
               .resultFormat(ResultFormatType.CSV)
               .result("results/" ++ outputFile)
               .build

    val runner = new Runner(opts) // full access to all JMH features, you can also provide a custom output Format here
    runner.run() // actually run the benchmarks
  }
}
