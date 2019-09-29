package dotty.tools.benchmarks.tuples

import org.openjdk.jmh.annotations._
import scala.runtime.DynamicTuple

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
    var t: Tuple = ()
    for (i <- 1 to size)
      t = "string" *: t
    t.asInstanceOf[NonEmptyTuple]
  }

  @Param(Array("1", "10", "25", "50", "75", "99"))
  var index: Int = _

  @Benchmark
  def baseline(): Unit = {}

  // Apply benchmarks, doesn't work for some reason
  // @Benchmark
  // def tupleApply(): Unit = {
  //   DynamicTuple.dynamicApply(tuple, index)
  // }
}
