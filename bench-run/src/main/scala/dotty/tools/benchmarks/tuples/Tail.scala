package dotty.tools.benchmarks.tuples

import org.openjdk.jmh.annotations._

@State(Scope.Thread)
class Tail {
  @Param(Array("1"))
  var size: Int = _
  var tuple: NonEmptyTuple = _
  var array: Array[Object] = _

  @Setup
  def setup(): Unit = {
    tuple = "elem" *: Tuple()

    for (i <- 1 until size)
      tuple = "elem" *: tuple

    array = Array.fill(size)("elem")
  }

  @Benchmark
  def tupleTail(): Tuple = {
    runtime.Tuples.tail(tuple)
  }

  @Benchmark
  def arrayTail(): Array[Object] = {
    array.tail
  }
}
