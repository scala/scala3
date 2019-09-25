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

    val opts = new OptionsBuilder()
               .shouldFailOnError(true)
               .jvmArgs("-Xms2G", "-Xmx2G")
               .mode(Mode.AverageTime)
               .timeUnit(TimeUnit.NANOSECONDS)
               .warmupIterations(warmup)
               .measurementIterations(iterations)
               .forks(forks)
               .build

    val runner = new Runner(opts) // full access to all JMH features, you can also provide a custom output Format here
    runner.run() // actually run the benchmarks
  }
}

@State(Scope.Thread)
class ConsConcatBenchmarks {
  @Param(Array("10", "20"))
  var size: Int = _
  var tuple: Tuple = _

  @Setup
  def setup(): Unit = {
    tuple = ()

    for (i <- 1 to size)
      tuple = "string" *: tuple
  }

  @Benchmark
  def baseline(): Unit = {}

  // Cons benchmarks
  @Benchmark
  def tupleCons(): Tuple = {
    "string" *: tuple
  }

  @Benchmark
  def dynamicTupleCons(): Tuple = {
    DynamicTuple.dynamicCons("string", tuple)
  }

  // Concat benchmarks
  @Benchmark
  def tupleConcat(): Tuple = {
   tuple ++ tuple
  }

  @Benchmark
  def dynamicTupleConcat(): Tuple = {
    DynamicTuple.dynamicConcat(tuple, tuple)
  }

  // Interesting behaviour : I expect the two following to be the same because of TupleOptimizations.scala
  @Benchmark
  def tupleConcat2(): Tuple = {
    ("string", "string") ++ ("string", "string")
  }

  @Benchmark
  def tupleNoConcat(): Tuple = {
    ("string", "string", "string", "string")
  }
}

@State(Scope.Thread)
class ApplyBenchmarks {
  val size: Int = 100

  val tuple: NonEmptyTuple = {
    var t = ()
    for (i <- 1 to size)
      t = "string" *: t
    t.asInstanceOf[NonEmptyTuple]
  }

  @Param(Array("1", "10", "25", "50", "75", "99"))
  var index: Int = _

  // Apply benchmarks, doesn't work for some reason
  // @Benchmark
  // def tupleApply(): Unit = {
  //   DynamicTuple.dynamicApply(tuple, index)
  // }
}
