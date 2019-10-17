package dotty.tools.benchmarks

import org.openjdk.jmh.results.RunResult
import org.openjdk.jmh.runner.Runner
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.results.format._
import org.openjdk.jmh.runner.options._
import java.util.concurrent.TimeUnit

import scala.io.Source

object Bench {
  def main(args: Array[String]): Unit = {
    val (intArgs, args1) = args.span(x => try { x.toInt; true } catch { case _: Throwable => false } )

    val warmup = if (intArgs.length > 0) intArgs(0).toInt else 20
    val iterations = if (intArgs.length > 1) intArgs(1).toInt else 20
    val forks = if (intArgs.length > 2) intArgs(2).toInt else 1

    if (args1.isEmpty) {
      println("You should specify which benchmarks to run.")
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
      .resultFormat(ResultFormatType.CSV)

    if (args1.length > 1 && args1(1) != "--") {
      for ((param, values) <- paramsFromFile(args1(1)))
        builder = builder.param(param, values: _*)
    }

    if (args1.length > 2) {
      builder = builder.result(args1(2))
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
