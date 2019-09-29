package dotty.tools.benchmarks

import org.openjdk.jmh.results.RunResult
import org.openjdk.jmh.runner.Runner
import org.openjdk.jmh.runner.options.OptionsBuilder
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.results.format._
import java.util.concurrent.TimeUnit

import scala.io.Source

object Bench {
  def main(args: Array[String]): Unit = {
    val (intArgs, args1) = args.span(x => try { x.toInt; true } catch { case _: Throwable => false } )

    val warmup = if (intArgs.length > 0) intArgs(0).toInt else 30
    val iterations = if (intArgs.length > 1) intArgs(1).toInt else 20
    val forks = if (intArgs.length > 2) intArgs(2).toInt else 1

    val benchmarks = if (args1.length > 0) args1(0) else ".*"
    val outputFile = if (args1.length > 2) args1(2) else "output.csv"

    var builder = new OptionsBuilder()
               .shouldFailOnError(true)
               .jvmArgs("-Xms2G", "-Xmx2G")
               .mode(Mode.AverageTime)
               .timeUnit(TimeUnit.NANOSECONDS)
               .warmupIterations(warmup)
               .measurementIterations(iterations)
               .forks(forks)
               .include(benchmarks)
               .resultFormat(ResultFormatType.CSV)
               .result(outputFile)

    if (args1.length > 1) {
      for ((param, values) <- paramsFromFile(args1(1)))
        builder = builder.param(param, values: _*)
    }

    val runner = new Runner(builder.build) // full access to all JMH features, you can also provide a custom output Format here
    runner.run // actually run the benchmarks
  }

  def paramsFromFile(file: String): Array[(String, Array[String])] = {
    Source.fromFile(file).getLines.toArray.map { l =>
      val Array(param, values) = l split ':'
      (param, values split ',')
    }
  }
}
