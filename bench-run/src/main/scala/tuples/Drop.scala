package dotty.tools.benchmarks.tuples

import org.openjdk.jmh.annotations._
import scala.runtime.DynamicTuple

@State(Scope.Thread)
class Drop {
  @Param(Array("0"))
  var size: Int = _
  var tuple: Tuple = _
  var array: Array[Object] = _
  var half: Int = _

  @Setup
  def setup(): Unit = {
    tuple = ()
    half = size / 2

    for (i <- 1 to size)
      tuple = "elem" *: tuple

    array = new Array[Object](size)
  }

  @Benchmark
  def tupleDrop(): Tuple = {
    DynamicTuple.dynamicDrop(tuple, half)
  }

  @Benchmark
  def arrayDrop(): Array[Object] = {
    array.drop(half)
  }
}
