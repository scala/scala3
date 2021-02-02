package dotty.tools.benchmarks.tuples

import org.openjdk.jmh.annotations._

@State(Scope.Thread)
class Take {
  @Param(Array("0"))
  var size: Int = _
  var tuple: Tuple = _
  var array: Array[Object] = _
  var half: Int = _

  @Setup
  def setup(): Unit = {
    tuple = Tuple()
    half = size / 2

    for (i <- 1 to size)
      tuple = "elem" *: tuple

    array = new Array[Object](size)
  }

  @Benchmark
  def tupleTake(): Tuple = {
    runtime.Tuples.take(tuple, half)
  }

  @Benchmark
  def arrayTake(): Array[Object] = {
    array.take(half)
  }
}
