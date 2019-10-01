package dotty.tools.benchmarks.tuples

import org.openjdk.jmh.annotations._
import scala.runtime.DynamicTuple

@State(Scope.Thread)
class Cons {
  @Param(Array("0"))
  var size: Int = _
  var tuple: Tuple = _
  var array: Array[Object] = _

  @Setup
  def setup(): Unit = {
    tuple = ()

    for (i <- 1 to size)
      tuple = "elem" *: tuple

    array = Array.fill(size)("elem")
  }

  @Benchmark
  def baseline(): Unit = {}

  @Benchmark
  def normal(): Tuple = {
    "elem" *: tuple
  }

  @Benchmark
  def inlined(): Tuple = {
    DynamicTuple.dynamicCons("elem", tuple)
  }

  // This part is here to measure the performance of dong a cons on arrays directly
  @Benchmark
  def arrayCons(): Array[Object] = {
    DynamicTuple.cons$Array("elem", array)
  }
}
